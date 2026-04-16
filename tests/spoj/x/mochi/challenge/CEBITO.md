# SPOJ CEBITO - The Secret Recipe

https://www.spoj.com/problems/CEBITO/

Build and output a decision tree strategy for finding which witch holds a recipe passed down a dynasty tree.

## Approach

**DFS from root with children sorted by ascending subtree weight.**

The dynasty forms a rooted tree (root = oldest witch who always had the recipe). Strategy: start at the root, and for each witch visited, branch into children in order of increasing subtree total-chat-time. This greedy ordering minimizes the expected worst-case time.

### Output format

The flat output format encodes the decision tree recursively:
- Print the witch to visit (name token)
- For each child in sorted order: print child's name (the response token, i.e. "she passed to child") then recurse into the child's subtree
- Leaf nodes (no children) are terminal — just print their name, no further responses

For root's children, we only output responses for each child's name (no NEVER, since root always had the recipe and can only pass to one child). For non-root nodes reached via a parent-says-child response, possible answers are only that node's children names (no NEVER, since we know the recipe was in this lineage).

### Implementation

1. Parse all k witches, building: names, chat times, parent names.
2. Build name→index map and CSR adjacency (child_start, children_flat).
3. Compute subtree weights bottom-up (reverse input order since children come after parents).
4. Insertion-sort each node's children by subtree weight ascending.
5. Iterative DFS from root using explicit node+iterator stacks.

### Scoring

Score = t/T where t = most talkative witch's chat time (fixed), T = worst-case strategy time. Sorting children by subtree weight ascending minimizes T for the worst case.

## Notes

- Names are exactly 5 letters (1 uppercase + 4 lowercase).
- k < 10^6; input is topologically ordered (parents before children).
- Output may not exceed 30MB; each witch name appears at most twice.
- The "NEVER" response token is never needed in this DFS-from-root strategy since every visited witch was in the recipe chain.
