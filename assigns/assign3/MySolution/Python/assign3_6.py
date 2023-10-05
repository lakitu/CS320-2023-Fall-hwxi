import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

class mylist:
    def __init__ (self):
        self.val = None
        self.child1 = None
        self.child2 = None
        self.type = "nil"
    def cons (self, i, list):
        self.val = i
        self.child1 = list
        self.type = "cons"
    def snoc (self, list, i):
        self.val = i
        self.child2 = list
        self.type = "snoc"
    def append2 (self, list1, list2):
        self.child1 = list1
        self.child2 = list2
        self.type = "append2"
    def reverse (self, list):
        self.child1 = list
        self.type = "reverse"
    def getType (self):
        return self.type
    def getValue (self):
        return self.val
    def getChild1 (self):
        return self.child1
    def getChild2 (self):
        return self.child2

def mylist_nil():
    return mylist()

def mylist_cons(i, list):
    newList = mylist()
    newList.cons(i, list)
    return newList

def mylist_snoc(list, i):
    newlist = mylist()
    newlist.snoc(list, i)
    return newlist

def mylist_append2(list1, list2):
    newlist = mylist()
    newlist.append2(list1, list2)
    return newlist

def mylist_reverse(list):
    newlist = mylist()
    newlist.reverse(list)
    return newlist

def mylist_foreach(list, work):
    type = list.getType()
    if type == "nil":
        return
    if type == "cons":
        work(list.getValue())
        mylist_foreach(list.getChild1(), work)
        return
    if type == "snoc":
        mylist_foreach(list.getChild2(), work)
        work(list.getValue())
        return
    if type == "reverse":
        mylist_foreach(list.getChild1(), work)
        return
    if type == "append2":
        mylist_rforeach(list.getChild2(), work)
        mylist_rforeach(list.getChild1(), work)
        return

def mylist_rforeach(list, work):
    type = list.getType()
    if type == "nil":
        return
    if type == "cons":
        mylist_foreach(list.getChild1(), work)
        work(list.getValue())
        return
    if type == "snoc":
        work(list.getValue())
        mylist_foreach(list.getChild2(), work)
        return
    if type == "reverse":
        mylist_rforeach(list.getChild1(), work)
        return
    if type == "append2":
        mylist_foreach(list.getChild1(), work)
        mylist_foreach(list.getChild2(), work)
        return
    
