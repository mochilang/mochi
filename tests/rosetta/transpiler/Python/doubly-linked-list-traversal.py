# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-26 19:54 +0700
import sys
sys.set_int_max_str_digits(0)

nodes = {}
head = 0 - 1
tail = 0 - 1
def listString():
    if head == 0 - 1:
        return "<nil>"
    r = "[" + nodes.get(head).get("value")
    id = int(nodes.get(head).get("next"))
    while id != 0 - 1:
        r = r + " " + nodes.get(id).get("value")
        id = int(nodes.get(id).get("next"))
    r = r + "]"
    return r
print(listString())
nodes[0] = {"value": "A", "next": 0 - 1, "prev": 0 - 1}
head = 0
tail = 0
nodes[1] = {"value": "B", "next": 0 - 1, "prev": 0}
nodes.get(0)["next"] = 1
tail = 1
print(listString())
nodes[2] = {"value": "C", "next": 1, "prev": 0}
nodes.get(1)["prev"] = 2
nodes.get(0)["next"] = 2
print(listString())
out = "From tail:"
id = tail
while id != 0 - 1:
    out = out + " " + nodes.get(id).get("value")
    id = int(nodes.get(id).get("prev"))
print(out)
