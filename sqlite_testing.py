import sqlite3
import os
import re
import numpy
import fractions
#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      Kevin
#
# Created:     08/09/2020
# Copyright:   (c) Kevin 2020
# Licence:     <your licence>
#-------------------------------------------------------------------------------

#currDB = "recipes1.db"
#conn = sqlite3.connect(currDB)

my_dir = 'C:\\Games\\Steam\\steamapps\\common\\Factorio\\data\\base\\'

"""Transforms a file name into the file path"""
def myPath(s):
    return os.path.join(my_dir, s)

def connectTo(databaseName):
    databasePath = myPath(databaseName + ".db")
    try:
        conn = sqlite3.connect(databasePath)
    except:
        conn.close()
        raise
    return conn

def parameterGroup(n):
    return "("+"?,"*(n-1) + "?)"

def sqlTuple(t):
    return "(" + ",".join(t) + ")"

def sqlTupleString(t):
    tb = ("'"+ti.replace("'", "''")+"'" for ti in t)
    return "(" + ",".join(tb) + ")"

def loadTableIntoCursor(cursor, a, table_name):
    a = tuple(sqliteConvert(a[i]) for i in range(len(a)))
    try:
        cursor.execute("insert into {0} values {1}".format(
                            table_name,
                            parameterGroup(len(a))),
                        a)
    except sqlite3.IntegrityError as e:
        if str(e).startswith("UNIQUE constraint failed: "):
            print("WARNING: duplicate entry attempted:"+str(a))
        else:
            print(a)
            raise

def updateTableIntoCursor(cursor, a):
    v = sqliteConvert(a[2])
    res ="update "+a[0]+" set "+a[1]+" = (?)"
    cursor.execute(res, (v,))

def sqliteConvert(v):
    if v == "NULL":
        return None
    elif v[0] == "'" and v[-1] == "'":
        return v[1:-1].replace("''", "'")
    else:
        try:
            f = float(v)
            if f.is_integer():
                return int(f)
            else:
                return f
        except ValueError:
            return v

def sqliteClear(filename, conn):
    if not conn:
        conn = sqlite3.connect(filename)
    conn.close()
    if os.path.isfile(filename):
        os.remove(filename)

def sqliteFile(conn, filename):
    cursor = conn.cursor()
    with open(filename, "r") as file:
        cursor.executescript(file.read())
    cursor.close()

def sqliteLine(conn, statement):
    cursor = conn.cursor()
    cursor.execute(statement)
    return cursor_generator(cursor)

def loadFileIntoCursor(cursor, filename, loader):
    cursor.execute("begin;")
    with open(filename, "r") as file:
        for line in file:
            a = tuple(line.strip().split(","))
            loader(cursor, a)
    cursor.execute("commit;")

def loadStringIntoCursor(cursor, s1, s2):
    loadFileIntoCursor(cursor,
                    myPath(s1),
                    lambda cursor, a: loadTableIntoCursor(cursor, a, s2))

def loadDataIntoConnection(conn):
    sqliteFile(conn, myPath("recipe_tables_init.txt"))
    cursor = conn.cursor()
    loadStringIntoCursor(cursor, "recipe_list.txt", "recipes")
    loadStringIntoCursor(cursor, "recipe_item_list.txt", "recipe_items")
    loadStringIntoCursor(cursor, "item_list.txt", "items")
    loadStringIntoCursor(cursor, "structure_list.txt", "structures")
    loadStringIntoCursor(cursor, "structure_crafting_categories.txt",
        "structure_crafting_categories")
    """loadFileIntoCursor(myPath("other_list.txt"),
                        loadOtherIntoCursor, cursor)
    loadFileIntoCursor(myPath("structure_list.txt"),
                        loadOtherIntoCursor, cursor)"""
    loadFileIntoCursor(cursor, myPath("table_updates.txt"),
                        updateTableIntoCursor)
    conn.commit()

def loadDataIntoDatabase(databaseName):
    databasePath = myPath(databaseName + ".db")
    sqliteClear(databasePath, None)
    try:
        conn = sqlite3.connect(databasePath)
        loadDataIntoConnection(conn)
    except:
        conn.close()
        raise
    conn.close()

currDatabase = "tes"

def databaseStart(string):
    currDatabase = string
    return sqlite3.connect(myPath(string+".db"))

def databaseOpConn(conn, cursorFunc, *arg):
    cursorFunc(conn.cursor())

def databaseOpString(string, cursorFunc, *arg):
    conn = databaseStart(string)
    cursor = conn.cursor()
    try:
        res = cursorFunc(cursor, *arg)
    except:
        conn.close()
        raise
    conn.close()
    return res

def databaseOp(cursorFunc, *arg):
    databaseOpString(currDatabase, cursorFunc, *arg)

####################################

#picks whichever item in L is earliest in ranking, or the first if none is
def pickBest(L, ranking):
    best_ranking = None
    ranked = L[0]
    for a in L:
        for i, b in enumerate(ranking):
            if a == b and (best_ranking is None or best_ranking > i):
                best_ranking = i
                ranked = a
    return ranked

def testPickBest():
    assert(pickBest([1], [])==1)
    assert(pickBest([1], [2])==1)
    assert(pickBest([3], [3])==3)
    assert(pickBest([1, 2], [])==1)
    assert(pickBest([1, 2], [2])==2)
    assert(pickBest([1, 2], [1])==1)
    assert(pickBest([1, 2], [2, 1])==2)
    assert(pickBest([1, 5, 7], [7])==7)
    assert(pickBest([1, 5, 7], [7, 1])==7)
    assert(pickBest([1, 5, 7], [2, 1])==1)
    assert(pickBest([1, 5, 7], [8, 9, 5, 7])==5)


#############################################

def getFuel(cursor):
    return list(cursor.execute("select name, fuel_category, fuel_value, \
    fuel_acceleration_multiplier, fuel_top_speed_multiplier, burnt_result from items \
    where fuel_category is not null"))

def getEnergyRequired(cursor, recipe):
    cursor.execute("select energy_required from recipes where name=(?)",
        (recipe,))
    return cursor_generator(cursor)[0][0]

def getRecipeByOutput(cursor, ss):
    return list(i[0] for i in cursor.execute("select distinct recipe_name from recipe_items \
    where item_name in {0} and input_or_output = 'o'".format(
    sqlTupleString(ss))))


def main():
    pass

if __name__ == '__main__':
    main()
