// Generated by Mochi TypeScript compiler

function exist(board: Array<Array<string>>, word: string): boolean {
  let m: number = board.length;
  (globalThis as any).m = m;
  if ((m == 0)) {
    return false;
  }
  let n: number = board[0].length;
  (globalThis as any).n = n;
  let visited: Array<Array<boolean>> = [];
  (globalThis as any).visited = visited;
  let r: number = 0;
  (globalThis as any).r = r;
  while ((r < m)) {
    let row: Array<boolean> = [];
    (globalThis as any).row = row;
    let c: number = 0;
    (globalThis as any).c = c;
    while ((c < n)) {
      row = row.concat([false]);
      c = c + 1;
    }
    visited = visited.concat([row]);
    r = r + 1;
  }
  function dfs(r: number, c: number, idx: number): boolean {
    if ((idx == word.length)) {
      return true;
    }
    if (((((r < 0) || (r >= m)) || (c < 0)) || (c >= n))) {
      return false;
    }
    if (visited[r][c]) {
      return false;
    }
    if ((board[r][c] != _indexString(word, idx))) {
      return false;
    }
    visited[r][c] = true;
    if (
      (((dfs(r + 1, c, idx + 1) || dfs(r - 1, c, idx + 1)) ||
        dfs(r, c + 1, idx + 1)) || dfs(r, c - 1, idx + 1))
    ) {
      visited[r][c] = false;
      return true;
    }
    visited[r][c] = false;
    return false;
  }
  for (let i: number = 0; i < m; i++) {
    for (let j: number = 0; j < n; j++) {
      if (dfs(i, j, 0)) {
        return true;
      }
    }
  }
  return false;
}

function findWords(
  board: Array<Array<string>>,
  words: Array<string>,
): Array<string> {
  let found: Array<string> = [];
  (globalThis as any).found = found;
  for (const w of words) {
    if (exist(board, w)) {
      found = found.concat([w]);
    }
  }
  return found;
}

function test_example(): void {
  let result: Array<string> = (() => {
    const _src = findWords(board, words);
    let _items = [];
    for (const w of _src) {
      _items.push(w);
    }
    let _pairs = _items.map((it) => {
      const w = it;
      return { item: it, key: w };
    });
    _pairs.sort((a, b) => {
      const ak = a.key;
      const bk = b.key;
      if (typeof ak === "number" && typeof bk === "number") return ak - bk;
      if (typeof ak === "string" && typeof bk === "string") {
        return ak < bk
          ? -1
          : (ak > bk ? 1 : 0);
      }
      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
    });
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const w of _items) {
      _res.push(w);
    }
    return _res;
  })();
  (globalThis as any).result = result;
  if (
    !(_equal(result, [
      "eat",
      "oath",
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  let board: Array<Array<string>> = [
    [
      "o",
      "a",
      "a",
      "n",
    ],
    [
      "e",
      "t",
      "a",
      "e",
    ],
    [
      "i",
      "h",
      "k",
      "r",
    ],
    [
      "i",
      "f",
      "l",
      "v",
    ],
  ];
  (globalThis as any).board = board;
  let words: Array<string> = [
    "oath",
    "pea",
    "eat",
    "rain",
  ];
  (globalThis as any).words = words;
  test_example();
}
function _equal(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_equal(a[i], b[i])) return false;
    return true;
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) {
      if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) {
        return false;
      }
    }
    return true;
  }
  return a === b;
}

function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
