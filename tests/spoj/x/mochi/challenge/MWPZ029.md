# SPOJ MWPZ029 - Rooms and Pictures

https://www.spoj.com/problems/MWPZ029/

N rooms form a functional graph: each room i has a picture pointing to room f[i].
People teleport every minute; eventually the number of occupied rooms stabilises.
The stable count equals the number of nodes that lie on cycles of the functional graph.

## Approach

Walk each unvisited node, colouring nodes 1 (in-progress) while on the current path and
2 (done) once finished. When a walk hits a node already coloured 1, we've closed a cycle;
count all nodes from that entry point to the end of the path. Nodes coloured 2 contribute
no new cycles. Total cycle-node count is the answer.
