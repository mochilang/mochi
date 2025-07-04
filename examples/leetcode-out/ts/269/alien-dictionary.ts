// Generated by Mochi TypeScript compiler

function alienOrder(words: Array<string>): string {
  let graph: Record<string, Array<string>> = {};
  (globalThis as any).graph = graph;
  let indegree: Record<string, number> = {};
  (globalThis as any).indegree = indegree;
  for (const w of words) {
    for (const ch of w) {
      if ((!(Object.prototype.hasOwnProperty.call(graph, String(ch))))) {
        graph[ch] = [];
      }
      if ((!(Object.prototype.hasOwnProperty.call(indegree, String(ch))))) {
        indegree[ch] = 0;
      }
    }
  }
  let i: number = 0;
  (globalThis as any).i = i;
  while ((i < (words.length - 1))) {
    let w1: string = words[i];
    (globalThis as any).w1 = w1;
    let w2: string = words[i + 1];
    (globalThis as any).w2 = w2;
    let j: number = 0;
    (globalThis as any).j = j;
    let found: boolean = false;
    (globalThis as any).found = found;
    while (((j < w1.length) && (j < w2.length))) {
      let c1: string = _indexString(w1, j);
      (globalThis as any).c1 = c1;
      let c2: string = _indexString(w2, j);
      (globalThis as any).c2 = c2;
      if ((c1 != c2)) {
        let neighbors: Array<string> = graph[c1];
        (globalThis as any).neighbors = neighbors;
        let exists: boolean = false;
        (globalThis as any).exists = exists;
        for (const n of neighbors) {
          if ((n == c2)) {
            exists = true;
          }
        }
        if ((!exists)) {
          graph[c1] = neighbors.concat([c2]);
          indegree[c2] = indegree[c2] + 1;
        }
        found = true;
        break;
      }
      j = j + 1;
    }
    if (((!found) && (w1.length > w2.length))) {
      return "";
    }
    i = i + 1;
  }
  let queue: Array<string> = [];
  (globalThis as any).queue = queue;
  for (const ch of Object.keys(indegree)) {
    if ((indegree[ch] == 0)) {
      queue = queue.concat([ch]);
    }
  }
  let order: string = "";
  (globalThis as any).order = order;
  let idx: number = 0;
  (globalThis as any).idx = idx;
  while ((idx < queue.length)) {
    let ch: string = queue[idx];
    (globalThis as any).ch = ch;
    idx = idx + 1;
    order = order + ch;
    for (const nxt of graph[ch]) {
      indegree[nxt] = indegree[nxt] - 1;
      if ((indegree[nxt] == 0)) {
        queue = queue.concat([nxt]);
      }
    }
  }
  if ((order.length != Object.keys(indegree).length)) {
    return "";
  }
  return order;
}

function test_example_1(): void {
  if (
    !(alienOrder([
      "wrt",
      "wrf",
      "er",
      "ett",
      "rftt",
    ]) == "wertf")
  ) throw new Error("expect failed");
}

function test_example_2(): void {
  if (
    !(alienOrder([
      "z",
      "x",
    ]) == "zx")
  ) throw new Error("expect failed");
}

function test_invalid_order(): void {
  if (
    !(alienOrder([
      "z",
      "x",
      "z",
    ]) == "")
  ) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_invalid_order();
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
