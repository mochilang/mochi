// Generated by Mochi v0.10.42 on 2025-07-28 10:10:23 GMT+7

let INF: number = 1000000;
export interface FWResult { dist: number[][]; next: number[][] }
function floydWarshall(graph: number[][]): FWResult {
  let n: number = (Array.isArray(graph) || typeof graph === 'string' ? graph.length : Object.keys(graph ?? {}).length);
  let dist: number[][] = [];
  let next: number[][] = [];
  let i: number = 0;
  while ((i < n)) {
    let drow: number[] = [];
    let nrow: number[] = [];
    let j: number = 0;
    while ((j < n)) {
      drow.push(graph[i][j]);
      if (((graph[i][j] < INF) && (i != j))) {
        nrow.push(j);
      } else {
        nrow.push(-1);
      }
      j = (j + 1);
    }
    dist.push(drow);
    next.push(nrow);
    i = (i + 1);
  }
  let k: number = 0;
  while ((k < n)) {
    let i: number = 0;
    while ((i < n)) {
      let j: number = 0;
      while ((j < n)) {
        if (((dist[i][k] < INF) && (dist[k][j] < INF))) {
          let alt: bigint = (dist[i][k] + dist[k][j]);
          if ((alt < BigInt(dist[i][j]))) {
            dist[i][j] = alt;
            next[i][j] = next[i][k];
          }
        }
        j = (j + 1);
      }
      i = (i + 1);
    }
    k = (k + 1);
  }
  return {dist, next};
}
function path(u: number, v: number, next: number[][]): number[] {
  if ((next[u][v] < 0)) {
    return [];
  }
  let p: number[] = [u];
  let x: number = u;
  while ((x != v)) {
    x = next[x][v];
    p.push(x);
  }
  return p;
}
function pathStr(p: number[]): string {
  let s: string = "";
  let i: number = 0;
  while ((i < (Array.isArray(p) || typeof p === 'string' ? p.length : Object.keys(p ?? {}).length))) {
    s = (s + _str((p[i] + 1)));
    if ((i < ((Array.isArray(p) || typeof p === 'string' ? p.length : Object.keys(p ?? {}).length) - 1))) {
      s = (s + " -> ");
    }
    i = (i + 1);
  }
  return s;
}
let n: number = 4;
let g: number[][] = [];
let i: number = 0;
var _nowSeed = 0;
var _nowSeeded = false;
{
  let s = "";
  if (typeof Deno !== "undefined") {
    try {
      s = Deno.env.get("MOCHI_NOW_SEED") ?? "";
    } catch (_e) {
      s = "";
    }
  } else if (typeof process !== "undefined") {
    s = process.env.MOCHI_NOW_SEED || "";
  }
  if (s) {
    const v = parseInt(s, 10);
    if (!isNaN(v)) {
      _nowSeed = v;
      _nowSeeded = true;
    }
  }
}
function _now(): number {
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
    return _nowSeed;
  }
  if (typeof Deno !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  if (typeof performance !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  return Date.now() * 1000;
}
function _mem(): number {
  if (typeof Deno !== 'undefined') {
    return (Deno.memoryUsage?.().rss ?? 0);
  }
  if (typeof process !== 'undefined') {
    return process.memoryUsage().rss;
  }
  return 0;
}
function _str(x: any): string {
  if (typeof x === 'number') {
    if (Object.is(x, -0)) return '-0';
    if (x === Infinity) return '+Inf';
    if (x === -Infinity) return '-Inf';
    if (Number.isNaN(x)) return 'NaN';
  }
  return String(x);
}
(() => {
  const _startMem = _mem()
  const _start = _now()
  for (let i = 0; i < n; i++) {
    let row: number[] = [];
    for (let j = 0; j < n; j++) {
      if ((i == j)) {
        row.push(0);
      } else {
        row.push(INF);
      }
    }
    g.push(row);
  }
  g[0][2] = -2;
  g[2][3] = 2;
  g[3][1] = -1;
  g[1][0] = 4;
  g[1][2] = 3;
  let res: FWResult = floydWarshall(g);
  console.log(_str("pair\tdist\tpath"));
  while ((i < n)) {
    let j: number = 0;
    while ((j < n)) {
      if ((i != j)) {
        let p: number[] = path(i, j, res.next);
        console.log(_str(((((((_str((i + 1)) + " -> ") + _str((j + 1))) + "\t") + _str(res.dist[i][j])) + "\t") + pathStr(p))));
      }
      j = (j + 1);
    }
    i = (i + 1);
  }
  const _end = _now()
  const _duration = _end - _start
  const _duration_us = Math.trunc(_duration / 1000)
  const _endMem = _mem()
  const _memory_bytes = Math.max(0, _endMem - _startMem)
  console.log(JSON.stringify({
    "duration_us": _duration_us,
    "memory_bytes": _memory_bytes,
    "name": "main"
  }, null, "  "))
})();

