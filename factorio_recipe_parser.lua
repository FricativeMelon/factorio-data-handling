
local inspect = require('ext-lib/inspect')
io.stdout:setvbuf('no')

--a list of tuples
__tuples = {}
setmetatable(__tuples, {__mode = "v"})

function listShallowMatch(a, ...)
    if #a ~= select("#", ...) then
        return false
    end
    for i, v in ipairs(a) do
        if b[i] ~= v then
            return false
        end
    end
    return true
end

function tuple(...)
    for i, tup in ipairs(__tuples) do
        if listShallowMatch(tup, ...) then
            return tup
        end
    end
    __tuples[#__tuples+1] = {...}
end

function recursive_table_mesh(t1, t2, meshFunc, defRes)
    local res = defRes
    local copyMapNow = {}
    local copyMapLater = {[t1]=t2}
    while next(copyMapLater) ~= nil do
        copyMapNow = copyMapLater
        copyMapLater = {}
        for k, v in next, copyMapNow, nil do
            --k is the map and v is the map to copy into it
            res = meshFunc(k, v, copyMapLater, res)
        end
    end
    return res
end

--a TypeTable is a table with Atom keys and Type values
--a TypeUnion is a table with Atom keys and PosInt values
--a TypeTableUnion is a table with Atom keys and TypeTable values

--a Type is a table with type, val, nilable fields
--    {type="table", val=TypeTable}
--    {type="union", val=TypeUnion}
--    {type="tableunion", val=TypeTableUnion, def=TypeTable}

-- a Thing is one of:
--  Atom
--  ThingTable

-- a ThingTable is a table with Atom keys and Thing values

--an Object is one of:
--  nil
--  Thing

-- Note: a Type is a ThingTable

function deep_copy_table_into(to, from, maps)
    for k,v in next, from, nil do
        if type(v) == "table" then
            local t = {}
            to[k] = t
            maps[t] = v
        else
            to[k] = v
        end
    end
end

-- Thing -> Thing
function deep_copy(orig)
    if type(orig) ~= "table" then
        return orig
    end
    local copy = {}
    recursive_table_mesh(copy, orig, deep_copy_table_into)
    return copy
end

-- ThingTable, ThingTable -> Boolean
-- returns whether they have the same keys
-- adds the corresponding values to the mapping to be compared later
function key_equal_tables(tab1, tab2, maps)
    for k, v in pairs(tab1) do
        if v ~= tab2[k] then
            if type(v) == "table" and type(tab2[k]) == "table" then
                maps[tab2[k]] = v
            else
                return false
            end
        end
    end
    for k, v in pairs(tab2) do
        if tab1[k] == nil then
            return false
        end
    end
    return true
end


function deep_equal_into_help(t1, t2, maps)
    if type(t1) == "table" and type(t2) == "table" then
        return key_equal_tables(t1, t2, maps)
    else
        return t1 == t2
    end
end

function deep_equal_into(t1, t2, maps, res)
    return res and deep_equal_into_help(t1, t2, maps)
end

function deep_equal(t1, t2)
    if type(t1) ~= "table" or type(t2) ~= "table" then
        return t1 == t2
    end
    return recursive_table_mesh(t1, t2, deep_equal_into, true)
end

function check_equal(a1, a2)
    assert(deep_equal(a1, a2), "\n" ..inspect(a1).."\n~=\n"..inspect(a2))
end

function check_not_equal(a1, a2, info)
    assert(not deep_equal(a1, a2), "\n" ..inspect(a1).."\n==\n"..inspect(a2))
end

function check_error(e, f, ...)
    local r, er = pcall(f, ...)
    assert(not r, "no error")
    assert(string.find(er, e), "Got error '"..er.."', expected something like '"..e.."'")
end


function test_distinct_list(L)
    for i=1, #L do
        local Li = L[i]
        if type(Li) == "function" then
            Li = Li()
        end
        for j=1, #L do
            local Lj = L[j]
            if type(Lj) == "function" then
                Lj = Lj()
            end
            if i==j then
                check_equal(Li, Lj)
            else
                check_not_equal(Li, Lj)
            end
        end
    end
end

function test_deep_equal_and_copy()
    local L = {function () return nil end, 0, 4, -4, 0.12, "", "ui", "abc45", 
                  "a b g \n", "nil", true, false, 1, "{}",
                  function () return {} end,
                  function () return {1} end,
                  function () return {"h"} end,
                  function () return {a=5} end,
                  function () return {["---"]=""} end,
                  function () return {{}, {}, {a=""}, 6, 7} end,
                  function () return {8} end,
                  function () return {9} end,
                  function () return {10} end,
                  function () return {11} end}
test_distinct_list(L)
    check_equal(04, 4)
    check_equal({a=2, b=4}, {b=4, a=2})
    for i=1,#L do
        local dc = deep_copy(L[i])
        if type(dc) == "table" then
            assert(L[i] ~= dc)
        end
        check_equal(L[i], dc)
    end
end

function add_table_to_table_type_val(tabtyval, tab, maps)
    for k, v in pairs(tab) do
        if tabtyval[k] == nil then
            tabtyval[k] = {type="union", val={}, nilable=true}
        end
        maps[tabtyval[k]] = v
    end
    for k, v in pairs(tabtyval) do
       if tab[k] == nil then
            v.nilable=true
        end
    end
end

function add_o_t_t_table_to_table_union_type(tut, tab, maps)
    local ty = tab.type
    if ty == nil then
        if tut.def == nil then
            tut.def = {}
        end
        add_table_to_table_type_val(tut.def, tab, maps)
    else
        if tut.val[ty] == nil then
            tut.val[ty] = {}
        end
        add_table_to_table_type_val(tut.val[ty], tab, maps)
        tut.val[ty].type = nil
    end
end


function add_o_t_t_table_to_union(un, tab, maps)
    if next(un.val) == nil then
        local ntab
        un.type="table"
        ntab = un.val
        for k, v in pairs(tab) do
            ntab[k] = {type="union", val={}, nilable=false}
            maps[ntab[k]] = v
        end
    else
        error("non empty union and table cannot be unified: \n"..inspect(un).."\n"..inspect(tab))
    end
end


function add_o_t_t_nontable_obj(ty, nontab, maps)
    if ty.type == "union" then
        if ty.val[nontab] == nil then
            ty.val[nontab] = 1
        else
            ty.val[nontab] = ty.val[nontab]+1
        end
    elseif ty.type == "table" then
        error("table and atom cannot be unified: \n"..inspect(ty).."\n"..inspect(nontab))
    end
end

-- modifies a type
-- obj cannot be nil
function add_o_t_t(ty, obj, maps)
    if type(obj) ~= "table" then
        add_o_t_t_nontable_obj(ty, obj, maps)
    elseif ty.type == "union" then
        add_o_t_t_table_to_union(ty, obj, maps)
    elseif ty.type == "table" then
        add_table_to_table_type_val(ty.val, obj, maps)
    end
end

-- Type Object -> Type
function add_object_to_type(ty, obj)
    local new_type = deep_copy(ty)
    if obj == nil then
        new_type.nilable = true
        return new_type
    end
    recursive_table_mesh(new_type, obj, add_o_t_t)
    return new_type
end

function test_add_object_to_type()
    local function cu(a, b) return {type="union", val=a, nilable=b} end
    local function cuv(a, b) return {type="union", val={[a]=1}, nilable=b} end
    local function ct(a, b) return {type="table", val=a, nilable=b} end

    do

    local function t(a1, a2, e)
        check_error(e, add_object_to_type, a1, a2)
    end

    t(
        cu({a=5, [2]=2}, false), {},
        "non empty union and table cannot be unified"
    )t(
        ct({}, false), 1,
        "table and atom cannot be unified"
    )t(
        ct({}, true), "a",
        "table and atom cannot be unified"
    )

    end

    do

    local function t(a1, a2, r)
        local a = add_object_to_type(a1, a2)
        check_equal(a, r)
    end

    t(
        cu({}, false), nil,
        cu({}, true)
    )t(
        cu({}, true), nil,
        cu({}, true)
    )t(
        cu({},      false), 1,
        cu({[1]=1}, false)
    )t(
        cu({},      true), 1,
        cu({[1]=1}, true)
    )t(
        cu({},    false), "a",
        cu({a=1}, false)
    )t(
        cu({},    true), "a",
        cu({a=1}, true)
    )t(
        cu({a=1}, false), "a",
        cu({a=2}, false)
    )t(
        cu({a=5}, false), "a",
        cu({a=6}, false)
    )t(
        cu({b=4},      true), "a",
        cu({a=1, b=4}, true)
    )t(
        cu({a=1, b=3}, false), "a",
        cu({a=2, b=3}, false)
    )t(
        cu({a=5, b=2}, true), "b",
        cu({a=5, b=3}, true)
    )t(
        cu({a=5, [2]=2},      false), "c",
        cu({a=5, [2]=2, c=1}, false)
    )t(
        cu({}, false), {},
        ct({}, false)
    )t(
        cu({}, true), {},
        ct({}, true)
    )t(
        cu({               }, false), {a=7},
        ct({a=cuv(7, false)}, false)
    )t(
        cu({               }, true), {a=7},
        ct({a=cuv(7, false)}, true)
    )t(
        ct({}, false), nil,
        ct({}, true)
    )t(
        ct({}, true), nil,
        ct({}, true)
    )t(
        ct({}, false), {},
        ct({}, false)
    )t(
        ct({              }, true), {a=1},
        ct({a=cuv(1, true)}, true)
    )t(
        ct({a=cu({[1]=1}, false)}, false), {a=1},
        ct({a=cu({[1]=2}, false)}, false)
    )t(
        ct({a=cuv(1, false)}, false), {},
        ct({a=cuv(1, true)},  false)
    )
    end
end

function add_counts(from, to)
    if to == nil then
        to = {}
    end
    for k, v in pairs(from) do
        if to[k] == nil then
            to[k] = v
        else
            to[k] = to[k] + v
        end
    end
    return to
end

function unify_union_and_table_types(ut, tt)
    if next(ut.val) == nil then
        ut.type="table"
        ut.val=deep_copy(tt.val)
        ut.nilable=ut.nilable or tt.nilable
    else
        error("non empty union and table cannot be unified: \n"..inspect(ut).."\n"..inspect(tt))
    end
end

function unify_t(ty1, ty2, maps)
    ty1.nilable = ty1.nilable or ty2.nilable
    if ty1.type == "union" and ty2.type == "union" then
        ty1.val=add_counts(ty2.val, add_counts(ty1.val))
    elseif ty1.type == "union" and ty2.type == "table" then
        unify_union_and_table_types(ty1, ty2)
    elseif ty1.type == "table" and ty2.type == "union" then
        unify_union_and_table_types(ty2, ty1)
    elseif ty1.type == "table" and ty2.type == "table" then
        add_table_to_table_type_val(ty1.val, ty2.val, maps)
    end
end

function unify_types(ty1, ty2)
    local new_type = deep_copy(ty1)
    recursive_table_mesh(new_type, ty2, unify_t)
    return new_type
end

function test_unify_types()
    local function cu(a, b) return {type="union", val=a, nilable=b} end
    local function cuv(a, b) return {type="union", val={[a]=1}, nilable=b} end
    local function ct(a, b) return {type="table", val=a, nilable=b} end

    do
    local function t(a1, a2, e)
        check_error(e, unify_types, a1, a2)
    end

    t(
        cu({a=5, [2]=2}, false), ct({a=cuv("a", false)}, false),
        "non empty union and table cannot be unified"
    )

    end

    do
    local function t(a1, a2, r)
        local a = unify_types(a1, a2)
        check_equal(a, r)
    end

    t(
        cu({}, false), cu({}, false),
        cu({}, false)
    )t(
        cu({}, true), cu({}, false),
        cu({}, true)
    )t(
        cu({}, false), cu({}, true),
        cu({}, true)
    )t(
        cu({}, true), cu({}, true),
        cu({}, true)
    )t(
        cu({u=1}, true), cu({}, false),
        cu({u=1}, true)
    )t(
        cu({u=1}, true), cu({u=1}, false),
        cu({u=2}, true)
    )t(
        cu({u=1}, true), cu({a=2}, false),
        cu({u=1, a=2}, true)
    )t(
        cu({}, true), cu({u=1}, false),
        cu({u=1}, true)
    )t(
        cu({}, false), ct({}, false),
        ct({}, false)
    )t(
        cu({}, true), ct({a=cu({}, true)}, false),
        ct({a=cu({}, true)}, true)
    )t(
        cu({}, false), ct({a=ct({a=cu({}, true)}, true)}, true),
        ct({a=ct({a=cu({}, true)}, true)}, true)
    )t(
        ct({}, false), ct({}, false),
        ct({}, false)
    )t(
        ct({}, true), ct({}, false),
        ct({}, true)
    )t(
        ct({}, false), ct({}, true),
        ct({}, true)
    )t(
        ct({}, true), ct({}, true),
        ct({}, true)
    )t(
        ct({}, false), ct({a=cuv(1, false)}, false),
        ct({a=cuv(1, true)}, false)
    )t(
        ct({a=cuv(1, false)}, false), ct({a=cuv(2, false)}, false),
        ct({a=cu({[1]=1, [2]=1}, false)}, false)
    )t(
        ct({a=cuv(1, true)}, false), ct({a=cuv(2, false)}, false),
        ct({a=cu({[1]=1, [2]=1}, true)}, false)
    )t(
        ct({a=cuv(1, false)}, false), ct({a=cuv(2, true)}, false),
        ct({a=cu({[1]=1, [2]=1}, true)}, false)
    )t(
        ct({a=cuv(1, false)}, false), ct({}, false),
        ct({a=cu({[1]=1}, true)}, false)
    )
    --[[{
        cu({               }, false), {a=7},
        ct({a=cuv(7, false)}, false)
    },{
        cu({               }, true), {a=7},
        ct({a=cuv(7, false)}, true)
    },{
        ct({}, false), nil,
        ct({}, true)
    },{
        ct({}, true), nil,
        ct({}, true)
    },{
        ct({}, false), {},
        ct({}, false)
    },{
        ct({              }, true), {a=1},
        ct({a=cuv(1, true)}, true)
    },{
        ct({a=cu({[1]=1}, false)}, false), {a=1},
        ct({a=cu({[1]=2}, false)}, false)
    },{
        ct({a=cuv(1, false)}, false), {},
        ct({a=cuv(1, true)},  false)
    }]]
    end
end

--return a self-referencing list of existing types in the data
--a type is one of:
--  string
--  number
--  table

-- a table is a map from string to type

function collapse_lists_into(t1, _, maps)
    if t1.type ~= "table" then
        return
    end
    local minLen = 0
    local maxLen = 0

    for k, v in pairs(t1.val) do
        if type(k) ~= "number" or k < 1 or k ~= math.floor(k) then
            return
        end
        if not v.nilable then
            minLen = math.max(minLen, k)
        end
        maxLen = math.max(maxLen, k)
    end

    if minLen ~= maxLen then
        t1.type = "list"
        local val = t1.val
        t1.val = {type="union", val={}, nilable=true}
        for i, v in ipairs(val) do
            t1.val = unify_types(t1.val, v)
        end
    end
end

-- Type -> Type
function collapse_lists(ty)
    local newType = deep_copy(ty)
    return recursive_table_mesh(newType, true, collapse_lists_into)
end
--{a=Atom, b=Atom, c = {a=Atom}}
--a TypeRestriction is a relation between fields and their values
--(field A has/does not have value B)* if (field C has/does not have value D)*
-- means that for every table unified to make the restriction set, the restrictions hold
-- for a singular table, this means (field of table has value of field in table)
-- need to determine which types the item can be and which types it always is
-- for example, given {} and any other table A, it can be any supertype of A, but the only type it always is is {}
-- given A and B, it can be any supertype of A or B, it always is every supertype of A and B

--a TypeRestriction is a relation between fields and their values
--(field A has/does not have value B)* if (field C has/does not have value D)*
-- means that for every table unified heree


local NIL = {}

local nil_proxy_mt = {
    __add = function (op1, op2) return 0 end,
    __sub = function (op1, op2) return 0 end,
    __mul = function (op1, op2) return 1 end,
    __div = function (op1, op2) return 1 end,
    __mod = function (op1, op2) return 0 end,
    __pow = function (op1, op2) return 1 end,
    __unm = function (op) return 0 end,
    __concat = function (op1, op2) return "" end,
    __len = function (op) return 0 end,
    __eq = function (op1, op2) return false end,
    __lt = function (op1, op2) return true end,
    __le = function (op1, op2) return true end,
    __index    = function(t,k) return NIL end,
    __newindex = function(t,k,v) end,
    __call = function(t) return NIL end
}

setmetatable(NIL, nil_proxy_mt)

NILATE = { __index = function(t,k) if rawget(t, k) == nil then return NIL else return rawget(t, k) end end}


--given a bunch of tables, determines the common formatting between all of them
function table_analysis()
end

--determines how close one table is to another
--the goal is to find clusters of tables that are all related strongly
--one option, view field as either 0 for not mapped or 1 for mapped, then calculate
--sum of absolute value of differences
--should be relative to total number of keys: {a=1, b=2} is closer to {a=1, b=3, c=4} than {} is to {}
--think of finding the type that both tables are, with mandatory and optional fields
--compareTables(t, t) == 0
--for every field that is in one table but not another table
function table_key_distance(t1, t2)
    local dist = 0
    local tot = 0
    for k, v in pairs(t1) do
        tot = tot + 1
        if t2[k] == nil then
            dist = dist + 1
        end
    end
    for k, v in pairs(t2) do
        tot = tot + 1
        if t1[k] == nil then
            dist = dist + 1
        end
    end
    if tot == 0 then
        return 0
    end
    return dist / tot
end

function test_table_key_distance()
    local function t(a1, a2, r)
        local a = table_key_distance(a1, a2)
        check_equal(a, r)
    end

    t(
        {}, {},
        0
    )t(
        {1}, {},
        1
    )t(
        {}, {2},
        1
    )t(
        {1}, {2},
        0
    )t(
        {1, 2}, {},
        1
    )t(
        {1, 2}, {3},
        1/3
    )t(
        {r=2}, {k=3},
        1
    )
end

--a cluster is valid if every table inside is closer to every other inside than
--every inside is to any table outside
--max(d(t1, t2) for t1, t2 in cluster)<min(d(t1, t2) for t1 in cluster, t2 out of cluster)
function cluster_table_check(cluster, other)
    local max_inner = 0
    local min_to_outer = 1
    for _, v in ipairs(cluster) do
        for _, w in ipairs(cluster) do
            local a = table_key_distance(v, w)
            if max_inner == nil or max_inner < a then
                max_inner = a
            end
        end
        for _, w in ipairs(other) do
            local a = table_key_distance(v, w)
            if min_to_outer == nil or min_to_outer > a then
                min_to_outer = a
            end
        end
    end
    return max_inner, min_to_outer
end

function test_cluster_table_check()
    local function t(a1, a2, r)
        local a, b = cluster_table_check(a1, a2)
        print(a, b)
        --check_equal(a, r)
    end

    t(
        {}, {},
        0, 1
    )t(
        {{}}, {},
        0, 1
    )t(
        {}, {{}},
        0, 1
    )t(
        {{}}, {{}},
        0, 0
    )t(
        {{}, {1}}, {},
        1, 1
    )t(
        {{}}, {{1}},
        0, 1
    )t(
        {}, {{}, {1}},
        0, 1
    )t(
        {{}, {1}}, {{}},
        1, 0
    )t(
        {}, {{}, {1}},
        0, 1
    )t(
        {}, {{}, {1}},
        0, 1
    )t(
        {}, {{}, {1}},
        0, 1
    )
end

--asserts that the type ta can include v as a member
--can be a single atom, representing the type only containing that atom
--can be a table:
--    the keys of the table represent required mappings to the type mapped to
--    any integer indices in the table represent special field connections:
--        {[sTT]="OPTIONAL", a=b, a2=b2, ...}:
--          represents a collection of mappings that can exist, and if they exist always exist together
--        {}

UNION = {}
TABLE_OR_NIL = {}

function add_to_Set(S, v)
    S[v] = true
end

function union_sets(S1, S2)
    res = {}
    for k, _ in pairs(S1) do
        res[k] = true
    end
    for k, _ in pairs(S2) do
        res[k] = true
    end
    return res
end

function union_multi_sets(S1, S2)
    for k, v in pairs(S2) do
        if S1[k] == nil then
            S1[k] = v
        else
            S1[k] = S1[k] + v
        end
    end
end

function union_tables(t1, t2, maps)
    for k, v in pairs(t2) do
        if t1[k] ~= nil then
            maps[t1[k]] = v
        end
    end
    for k, v in pairs(t1) do
        if t2[k] == nil then
            t1[k] = nil
        end
    end
end

typeIndex = {}

--types are:
-- Union (Nil, Table, Atom:boolean ...)
-- Table (Field:Type...)

--{nill=?, atoms=?, table=?}

-- gets the smallest type that contains both

function get_narrowest_supertype_into(t1, t2, maps)
    --t1.nill = t1.nill == true or t2.nill == true
    if t2.atoms ~= nil then
        if t1.atoms ~= nil then
            union_multi_sets(t1.atoms, t2.atoms)
        else
            t1.atoms = deep_copy(t2.atoms)
        end
    end 
    if t2.table ~= nil then
        if t1.table ~= nil then
            union_tables(t1.table, t2.table, maps)
        else
            t1.table = deep_copy(t2.table)
        end
    end
end

function get_narrowest_supertype(t1, t2)
    local newType = deep_copy(t1)
    recursive_table_mesh(newType, t2, get_narrowest_supertype_into)
    return newType
end

function test_get_narrowest_supertype()
    local function t(a1, a2, r)
        local a = get_narrowest_supertype(a1, a2)
        check_equal(a, r)
    end

    t(
        {}, {},
        {}
    )t(
        {}, {atoms={1}},
        {atoms={1}}
    )t(
        {atoms={1}}, {atoms={2}},
        {atoms={3}}
    )t(
        {atoms={1}}, {atoms={y=4}},
        {atoms={1, y=4}}
    )t(
        {table={}}, {},
        {table={}}
    )t(
        {table={}}, {table={y={atoms={"hi"}}}},
        {table={}}
    )t(
        {table={s={atoms={1, 1}}}}, {table={y={atoms={hi=1}}}},
        {table={}}
    )t(
        {table={s={atoms={1, 1}}}}, {table={s={atoms={hi=1}}}},
        {table={s={atoms={1, 1, hi=1}}}}
    )t(
        {table={s={atoms={1, 1}}}}, {atoms={1}},
        {table={s={atoms={1, 1}}}, atoms={1}}
    )
end

function get_all_types_into(tab, _, maps)

end

function get_all_types(tab)
    return recursive_table_mesh(tab, true, get_all_types_into, {})
end

test_deep_equal_and_copy()
test_table_key_distance()
--test_cluster_table_check()
test_add_object_to_type()
test_unify_types()
test_get_narrowest_supertype()

data_types = {}

--all tables subtype {type="table", val={}, nilable=false}
--{type="table", val=X, nilable=true} is a subtype of {type="table", val=X, nilable=true}

glob_data = {}

function add_data(data, ti)
    local ty = ti.type
    if data.type == nil then
        data.type = {type="union", val={}, nilable=false}
    end
    data[#(data)+1] = ti
    data.type = add_object_to_type(data.type, ti)
    --addToTableNested(data[ty].type, ti)
end

--[[data["extend"] = function (data, t)
    for i=1, #t do
        add_data(glob_data, t[i])
    end
end]]

--[[function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
        local loader = searcher(name)
          if type(loader) == 'function' then
            package.preload[name] = loader
            return true
          end
    end
    return false
  end
end]]

oldrequire = require

path = "C:\\Games\\Steam\\steamapps\\common\\Factorio\\data\\base\\"

package.path = package.path .. ";"..path.."?.lua"

require = function (s, b)
    local r, v = pcall(function () oldrequire(s) end)
    if r then
        if v == nil then
            return NIL
        else

            return v
        end
    end
    if b then
        print(s, r, v)
        print("double fail")
        return NIL
    end
    if type(v) == "string" then
        local start, last = string.find(v, "module '[^']*' not found")
        if start ~= nil then
          -- print(string.sub(v, start, last))
        else
            start, last = string.find(v, "[0-9]*: ambiguous syntax [(]function call x new statement[)]")
            if start ~= nil then
                local p = path..string.gsub(s, "[.]", "\\")..".lua"
                local file = io.open(p, "r")
                local a = file:read("*all")
                --a = string.gsub(a, "[}],[\n\t ]*[}]", "}}")
                a = string.gsub(a, "%-%-%[%[.-]]", "")
                a = string.gsub(a, "%-%-[^\n]*\n", "\n")
                a = string.gsub(a, "\n[\t ]*%(", "%(")
                --a = string.gsub(a, "[}]\n[)]", "})")
                file:close()
                assert(loadstring(a))()
                --require(new_file_name, true)
                --print(string.sub(v, start, last))
            else
                print(s, v)
            end
        end
    else
        print(s, v)
    end
    --print(s, v)
    return NIL
end

function sql_format(data)
    res = {}
    for k, v in pairs(data) do
        if v[1] == nil then
            v = "NULL"
        elseif v[2] == "string" then
            if type(v[1]) ~= "string" then
                v = tostring(v[1])
                --print(inspect(v[1]))
            else
                v = v[1]
            end
            v = string.gsub(v, "'", "''")
            v = "'" .. v .. "'"
        else
            v = v[1]
        end
        res[k] = v
    end
    --print(inspect(res))
    return table.concat(res, ",") .. "\n"
end

function process_IO(data, file, rname, i_or_o)
    for k, v in pairs(data) do
        if v.name == nil then
            v.name = v[1]
            v.amount = v[2]
            v.probability = 1
        elseif v.probability == nil then
            v.probability = 1
        end
        file:write(sql_format({{rname, "string"},
                               {v.name, "string"},
                               {i_or_o, "string"},
                               {v.amount, "integer"},
                               {v.probability, "real"}}))
    end
end

function clear_file(filename)
    f = io.open(filename, "w")
    f:write("")
    f:close()
end

function process_recipe(recipe)
    if recipe.normal ~= nil then
        for k,v in pairs(recipe.normal) do recipe[k] = v end
    end
    enabled_or_disabled = "e"
    if recipe.enabled == false then
        enabled_or_disabled = "d"
    end
    files.recipe_list:write(sql_format({{recipe.name, "string"},
                               {recipe.category or "crafting", "string"},
                               {recipe.energy_required or 0.5, "integer"},
                               {enabled_or_disabled, "string"}}))
    if recipe.result ~= nil then
        recipe.results = {{recipe.result, recipe.result_count or 1}}
    end
    process_IO(recipe.ingredients, files.recipe_item_list, recipe.name, "i")
    process_IO(recipe.results, files.recipe_item_list, recipe.name, "o")
end

function in_list(a, L)
    for i, v in ipairs(L) do
        if v == a then
            return i
        end
    end
    return nil
end

function convert_fuel_value(s)
    local lastTwo, val = s:sub(-2), tonumber(s:sub(1, #s-2))
    if lastTwo == "MJ" then
        return val
    elseif lastTwo == "GJ" then
        return 1000 * val
    end
end

item_structures = {}

function process_item(item)
    local fuel = item.fuel_value
    if fuel ~= nil then
        fuel = convert_fuel_value(fuel)
        item.fuel_acceleration_multiplier = item.fuel_acceleration_multiplier or 1
        item.fuel_top_speed_multiplier = item.fuel_acceleration_multiplier or 1
    end
    local hidden
    if inList("hidden", item.flags or {}) then
        hidden = "h"
    end
    files.item_list:write(sql_format({{item.name, "string"},
                                {item.stack_size, "integer"},
                                {item.subgroup, "string"},
                                {item.order, "string"},
                                {nil, "string"},
                                {hidden, "string"},
                                {item.fuel_category, "string"},
                                {fuel, "integer"},
                                {item.fuel_acceleration_multiplier, "string"},
                                {item.fuel_top_speed_multiplier, "string"},
                                {nil, "string"},
                                {item.equipment_grid, "string"},
                                {item.inventory_size_bonus, "integer"},
                                {nil, "integer"}
                            }))
    if item.burnt_result ~= nil then
        files.table_updates:write(sql_format({{"items"},
                                             {"burnt_result"},
                                             {item.name, "string"},
                                             {item.burnt_result, "string"}}))
    end
    if item.place_result ~= nil then
        item_structures[#item_structures+1] = item.place_result
        files.table_updates:write(sql_format({{"items"},
                                             {"place_result"},
                                             {item.name, "string"},
                                             {item.place_result, "string"}}))
    end
end

function process_other_object(v)
    files.other_list:write(sql_format({{v.type.."."..v.name, "string"}}))
end

function process_structure(structure)
    local mining_time, result
    if structure.minable ~= nil then
        mining_time=structure.minable.mining_time
        result=structure.minable.result
    end
    if structure.crafting_categories ~= nil then
        for i, v in ipairs(structure.crafting_categories) do
            files.structure_crafting_categories:write(sql_format({
                {structure.name, "string"},
                {v, "string"}
            }))
        end
    end
    files.structure_list:write(sql_format({{structure.name, "string"},
                                           {mining_time, "real"},
                                           {nil, "string"},
                                           {nil, "integer"},
                                           {structure.crafting_speed, "string"},
                                           {structure.speed, "string"},
                                           {structure.mining_speed, "string"},
                                           {structure.pumping_speed, "string"}}))

    if result ~= nil then
        files.table_updates:write(sql_format({{"structures"},
                                             {"result_item"},
                                             {structure.name, "string"},
                                             {result, "string"}}))
    end
end

function process_technology(tech)
    local upgrade = "n"
    if tech.upgrade then
        upgrade = "u"
    end
    files.technology_list:write(sql_format(
        {{tech.name, "string"},
        {tech.unit.count, "integer"},
        {tech.unit.time, "integer"},
        {upgrade, "string"}                                
    }))
end

data = {}
data.is_demo = false
types = {}
other_objects = {}
data["extend"] = function (data, t)
    for i, v in ipairs(t) do
        files.all_objects:write(inspect(v).."\n")
        local ty = v.type
        --print(ty)
        if ty == "recipe" then
            process_recipe(v)
        elseif ty == "item" then
            process_item(v)
        elseif ty == "technology" then
            process_technology(v)
        elseif ty == "item-group" or ty == "item-subgroup" or ty == "ammo-category" then
        else
            if types[ty] == nil then
                types[ty] = 1
            else
                types[ty] = types[ty]+1
            end
            other_objects[#other_objects+1] = v
        end
        --add_data(glob_data, v)
    end
end

files = {"recipe_list","recipe_item_list","item_list","table_updates",
"other_list","all_objects", "structure_list", "resist_list", "technology_list",
"tech_effect_list", "tech_prereq_list", "tech_ingredients_list", "structure_crafting_categories"}

function set_up_file(s)
    local n = "data\\"..s..".txt"
    clear_file(n)
    files[s]=io.open(n, "a")
end

function close_file(s)
    files[s]:close()
end

for i,v in ipairs(files) do
    set_up_file(v)
end

setmetatable(data, NILATE)

setmetatable(_G, NILATE)

dofile(path.."data.lua")

setmetatable(_G, nil)

for i, v in ipairs(other_objects) do
    if in_list(v.name, item_structures) then
        process_structure(v)
    else
        process_other_object(v)
    end
end

for i,v in ipairs(files) do
    close_file(v)
end

--print(inspect(types))

--print(inspect(glob_data))

--print(inspect(glob_data.type.val["technology"]))

require = oldrequire
--print(inspect(glob_data["recipe"].type))