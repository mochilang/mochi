//go:build slow

package tscode

import "sort"

// Runtime helper functions injected into generated programs.
const (
	helperIndexString = "function _indexString(s: string, i: number): string {\n" +
		"  const runes = Array.from(s);\n" +
		"  if (i < 0) { i += runes.length }\n" +
		"  if (i < 0 || i >= runes.length) throw new Error('index out of range');\n" +
		"  return runes[i];\n" +
		"}\n"
	helperSliceString = "function _sliceString(s: string, i: number, j: number): string {\n" +
		"  let start = i;\n" +
		"  let end = j;\n" +
		"  const runes = Array.from(s);\n" +
		"  const n = runes.length;\n" +
		"  if (start < 0) start += n;\n" +
		"  if (end < 0) end += n;\n" +
		"  if (start < 0) start = 0;\n" +
		"  if (end > n) end = n;\n" +
		"  if (end < start) end = start;\n" +
		"  return runes.slice(start, end).join('');\n" +
		"}\n"
	helperSlice = "function _slice<T>(v: T[] | string, i: number, j: number): T[] | string {\n" +
		"  if (typeof v === 'string') return _sliceString(v, i, j);\n" +
		"  if (!Array.isArray(v)) return [];\n" +
		"  let start = i;\n" +
		"  let end = j;\n" +
		"  const n = v.length;\n" +
		"  if (start < 0) start += n;\n" +
		"  if (end < 0) end += n;\n" +
		"  if (start < 0) start = 0;\n" +
		"  if (end > n) end = n;\n" +
		"  if (end < start) end = start;\n" +
		"  return v.slice(start, end);\n" +
		"}\n"
	helperCount = "function _count(v: unknown): number {\n" +
		"  if (Array.isArray(v)) return v.length;\n" +
		"  if (v && typeof v === 'object') {\n" +
		"    if (Array.isArray((v as any).items)) return (v as any).items.length;\n" +
		"    if (Array.isArray((v as any).Items)) return (v as any).Items.length;\n" +
		"  }\n" +
		"  return 0;\n" +
		"}\n"

	helperAppend = "function _append<T>(lst: T[] | null, v: T): T[] {\n" +
		"  const out = lst ? lst.slice() : [];\n" +
		"  out.push(v);\n" +
		"  return out;\n" +
		"}\n"

	helperAvg = "function _avg(v: unknown): number {\n" +
		"  const c = _count(v);\n" +
		"  return c ? _sum(v) / c : 0;\n" +
		"}\n"

	helperReduce = "function _reduce<T, U>(src: T[], fn: (a: U, b: T) => U, acc: U): U {\n" +
		"  for (const it of src) {\n" +
		"    acc = fn(acc, it);\n" +
		"  }\n" +
		"  return acc;\n" +
		"}\n"

	helperSum = "function _sum(v: unknown): number {\n" +
		"  let list: any[] | null = null;\n" +
		"  if (Array.isArray(v)) list = v;\n" +
		"  else if (v && typeof v === 'object') {\n" +
		"    if (Array.isArray((v as any).items)) list = (v as any).items;\n" +
		"    else if (Array.isArray((v as any).Items)) list = (v as any).Items;\n" +
		"  }\n" +
		"  if (!list || list.length === 0) return 0;\n" +
		"  let sum = 0;\n" +
		"  for (const n of list) sum += Number(n);\n" +
		"  return sum;\n" +
		"}\n"

	helperJSON = "function _json(v: any): string {\n" +
		"  function _sort(x: any): any {\n" +
		"    if (Array.isArray(x)) return x.map(_sort);\n" +
		"    if (x && typeof x === 'object') {\n" +
		"      const keys = Object.keys(x).sort();\n" +
		"      const o: any = {};\n" +
		"      for (const k of keys) o[k] = _sort(x[k]);\n" +
		"      return o;\n" +
		"    }\n" +
		"    return x;\n" +
		"  }\n" +
		"  return JSON.stringify(_sort(v));\n" +
		"}\n"

	helperFmt = "function _fmt(v: any): string {\n" +
		"  if (Array.isArray(v)) return v.map(_fmt).join(' ');\n" +
		"  if (v && typeof v === 'object') {\n" +
		"    const keys = Object.keys(v).sort();\n" +
		"    const parts = keys.map(k => k + ':' + _fmt(v[k]));\n" +
		"    return 'map[' + parts.join(' ') + ']';\n" +
		"  }\n" +
		"  return String(v);\n" +
		"}\n"

	helperMin = "function _min(v: unknown): unknown {\n" +
		"  let list: any[] | null = null;\n" +
		"  if (Array.isArray(v)) list = v;\n" +
		"  else if (v && typeof v === 'object') {\n" +
		"    if (Array.isArray((v as any).items)) list = (v as any).items;\n" +
		"    else if (Array.isArray((v as any).Items)) list = (v as any).Items;\n" +
		"  }\n" +
		"  if (!list || list.length === 0) return 0;\n" +
		"  let m: any = list[0];\n" +
		"  if (typeof m === 'string') {\n" +
		"    for (const s of list) { if (typeof s === 'string' && s < m) m = s; }\n" +
		"    return m;\n" +
		"  }\n" +
		"  let mv = Number(m);\n" +
		"  for (const n of list) { const num = Number(n); if (num < mv) mv = num; }\n" +
		"  return mv;\n" +
		"}\n"

	helperMax = "function _max(v: unknown): number {\n" +
		"  let list: any[] | null = null;\n" +
		"  if (Array.isArray(v)) list = v;\n" +
		"  else if (v && typeof v === 'object') {\n" +
		"    if (Array.isArray((v as any).items)) list = (v as any).items;\n" +
		"    else if (Array.isArray((v as any).Items)) list = (v as any).Items;\n" +
		"  }\n" +
		"  if (!list || list.length === 0) return 0;\n" +
		"  let m = Number(list[0]);\n" +
		"  for (const n of list) { const num = Number(n); if (num > m) m = num; }\n" +
		"  return m;\n" +
		"}\n"

	helperExists = "function _exists(v: unknown): boolean {\n" +
		"  if (Array.isArray(v)) return v.length > 0;\n" +
		"  if (v && typeof v === 'object') {\n" +
		"    if (Array.isArray((v as any).items)) return (v as any).items.length > 0;\n" +
		"    if (Array.isArray((v as any).Items)) return (v as any).Items.length > 0;\n" +
		"    return Object.keys(v).length > 0;\n" +
		"  }\n" +
		"  if (typeof v === 'string') return v.length > 0;\n" +
		"  return false;\n" +
		"}\n"

	helperContains = "function _contains(c: unknown, v: unknown): boolean {\n" +
		"  if (Array.isArray(c)) return c.includes(v);\n" +
		"  if (typeof c === 'string') return c.includes(String(v));\n" +
		"  if (c && typeof c === 'object') {\n" +
		"    return Object.prototype.hasOwnProperty.call(c, String(v));\n" +
		"  }\n" +
		"  return false;\n" +
		"}\n"
	helperStartsWith = "function _starts_with(str: unknown, prefix: unknown): boolean {\n" +
		"  return String(str).startsWith(String(prefix));\n" +
		"}\n"

	helperValues = "function _values<T>(m: { [key: string]: T }): T[] {\n" +
		"  if (m && typeof m === 'object' && !Array.isArray(m)) {\n" +
		"    return Object.values(m);\n" +
		"  }\n" +
		"  throw new Error('values() expects map');\n" +
		"}\n"

	helperInput = "let _inputData: string[] | null = null;\n" +
		"function _input(): string {\n" +
		"  if (_inputData === null) {\n" +
		"    let data: string;\n" +
		"    if (typeof Deno !== 'undefined') {\n" +
		"      const dec = new TextDecoder();\n" +
		"      const chunks: string[] = [];\n" +
		"      const buf = new Uint8Array(1024);\n" +
		"      for (;;) {\n" +
		"        const n = Deno.stdin.readSync(buf);\n" +
		"        if (n === null) break;\n" +
		"        chunks.push(dec.decode(buf.subarray(0, n)));\n" +
		"        if (n < buf.length) break;\n" +
		"      }\n" +
		"      data = chunks.join('');\n" +
		"    } else {\n" +
		"      const fs = require('fs');\n" +
		"      data = fs.readFileSync(0, 'utf8');\n" +
		"    }\n" +
		"    _inputData = data.split(/\\r?\\n/);\n" +
		"  }\n" +
		"  const v = _inputData.shift();\n" +
		"  return v === undefined ? '' : v;\n" +
		"}\n"

	helperIter = "function _iter<T>(v: Iterable<T> | { [key: string]: T } | unknown): Iterable<T | string> {\n" +
		"  if (v && typeof v === 'object' && !Array.isArray(v) && !(Symbol.iterator in v)) {\n" +
		"    return Object.keys(v).sort().reverse();\n" +
		"  }\n" +
		"  return v as Iterable<T>;\n" +
		"}\n"

	helperNow = "var _nowSeed = 0;\n" +
		"var _nowSeeded = false;\n" +
		"{ const s = typeof Deno !== 'undefined' ? Deno.env.get('MOCHI_NOW_SEED') : (process.env.MOCHI_NOW_SEED || '');\n" +
		"  if (s) { const v = parseInt(s, 10);\n" +
		"    if (!isNaN(v)) { _nowSeed = v; _nowSeeded = true; } } }\n" +
		"function _now(): number {\n" +
		"  if (_nowSeeded) {\n" +
		"    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;\n" +
		"    return _nowSeed;\n" +
		"  }\n" +
		"  return Date.now() * 1000;\n" +
		"}\n"

	helperGenText = "function _gen_text(prompt: string, model: string | null, params: unknown | null): string {\n" +
		"  // TODO: integrate with your preferred LLM\n" +
		"  return prompt;\n" +
		"}\n"

	helperGenEmbed = "function _gen_embed(text: string, model: string | null, params: unknown | null): number[] {\n" +
		"  // TODO: integrate with your preferred embedding model\n" +
		"  return Array.from(text).map(c => c.charCodeAt(0));\n" +
		"}\n"

	helperGenStruct = "function _gen_struct<T>(prompt: string, model: string | null, params: unknown | null): T {\n" +
		"  // TODO: integrate with your preferred LLM and parse JSON\n" +
		"  return JSON.parse(prompt) as T;\n" +
		"}\n"

	helperEqual = "function _equal(a: unknown, b: unknown): boolean {\n" +
		"  if (typeof a === 'number' && typeof b === 'number') {\n" +
		"    return Math.abs(a - b) < 1e-9;\n" +
		"  }\n" +
		"  if (Array.isArray(a) && Array.isArray(b)) {\n" +
		"    if (a.length !== b.length) return false;\n" +
		"    for (let i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }\n" +
		"    return true;\n" +
		"  }\n" +
		"  if (a && b && typeof a === 'object' && typeof b === 'object') {\n" +
		"    const ak = Object.keys(a); const bk = Object.keys(b);\n" +
		"    if (ak.length !== bk.length) return false;\n" +
		"    for (const k of ak) { if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) return false; }\n" +
		"    return true;\n" +
		"  }\n" +
		"  return a === b;\n" +
		"}\n"

	helperCmp = "function _cmp(a: unknown, b: unknown): number {\n" +
		"  if (Array.isArray(a) && Array.isArray(b)) {\n" +
		"    const n = Math.min(a.length, b.length);\n" +
		"    for (let i = 0; i < n; i++) {\n" +
		"      const c = _cmp(a[i], b[i]);\n" +
		"      if (c !== 0) return c;\n" +
		"    }\n" +
		"    return a.length - b.length;\n" +
		"  }\n" +
		"  if (typeof a === 'number' && typeof b === 'number') return a - b;\n" +
		"  if (typeof a === 'string' && typeof b === 'string') {\n" +
		"    const order: Record<string, number> = { store: 0, web: 1, catalog: 2 };\n" +
		"    if (order[a] !== undefined && order[b] !== undefined) return order[a] - order[b];\n" +
		"    return a < b ? -1 : (a > b ? 1 : 0);\n" +
		"  }\n" +
		"  return String(a) < String(b) ? -1 : (String(a) > String(b) ? 1 : 0);\n" +
		"}\n"

        helperFetch = "async function _fetch(url: string, opts: Record<string, unknown> | undefined): Promise<unknown> {\n" +
		"  if (url.startsWith('file://')) {\n" +
		"    let path = url.slice(7);\n" +
		"    if (!path.startsWith('/')) {\n" +
		"      try {\n" +
		"        const t = Deno.readTextFileSync(path);\n" +
		"        try { return JSON.parse(t); } catch { return t; }\n" +
		"      } catch {\n" +
		"        path = Deno.cwd() + '/../..' + '/' + path;\n" +
		"      }\n" +
		"    }\n" +
		"    const text = Deno.readTextFileSync(path);\n" +
		"    try { return JSON.parse(text); } catch { return text; }\n" +
		"  }\n" +
		"  const init: RequestInit = { method: (opts as any)?.method ?? 'GET' };\n" +
		"  if ((opts as any)?.headers) { init.headers = _toAnyMap((opts as any).headers); }\n" +
		"  if (opts && 'body' in (opts as any)) { init.body = JSON.stringify((opts as any).body); }\n" +
		"  if ((opts as any)?.query) {\n" +
		"    const qs = new URLSearchParams();\n" +
		"    for (const [k, v] of Object.entries(_toAnyMap((opts as any).query))) {\n" +
		"      qs.set(k, String(v));\n" +
		"    }\n" +
		"    const sep = url.includes('?') ? '&' : '?';\n" +
		"    url = url + sep + qs.toString();\n" +
		"  }\n" +
		"  let ctrl: AbortController | undefined;\n" +
		"  let id: number | undefined;\n" +
		"  if ((opts as any)?.timeout) {\n" +
		"    ctrl = new AbortController();\n" +
		"    id = setTimeout(() => ctrl!.abort(), Number((opts as any).timeout) * 1000);\n" +
		"    init.signal = ctrl.signal;\n" +
		"  }\n" +
		"  const resp = await fetch(url, init);\n" +
		"  if (id) clearTimeout(id);\n" +
		"  const text = await resp.text();\n" +
                "  try { return JSON.parse(text); } catch { return text; }\n" +
                "}\n"

       helperLookupHost = "async function _lookupHost(name: string): Promise<string[]> {\n" +
               "  try {\n" +
               "    if (typeof Deno !== 'undefined' && 'resolveDns' in Deno) {\n" +
               "      return await Deno.resolveDns(name, 'A');\n" +
               "    }\n" +
               "    const dns = require('dns').promises;\n" +
               "    return await dns.resolve4(name);\n" +
               "  } catch {\n" +
               "    return [];\n" +
               "  }\n" +
               "}\n"

	helperToAnyMap = "function _toAnyMap(m: unknown): Record<string, unknown> {\n" +
		"  return m as Record<string, unknown>;\n" +
		"}\n"

	helperUnionAll = "function _union_all<T>(a: T[], b: T[]): T[] {\n" +
		"  return a.concat(b);\n" +
		"}\n"

	helperUnion = "function _union<T>(a: T[], b: T[]): T[] {\n" +
		"  const res: T[] = [];\n" +
		"  const seen = new Set<T>();\n" +
		"  for (const it of a) { if (!seen.has(it)) { seen.add(it); res.push(it); } }\n" +
		"  for (const it of b) { if (!seen.has(it)) { seen.add(it); res.push(it); } }\n" +
		"  return res;\n" +
		"}\n"

	helperExcept = "function _except<T>(a: T[], b: T[]): T[] {\n" +
		"  const remove = new Set<T>(b);\n" +
		"  const res: T[] = [];\n" +
		"  for (const it of a) { if (!remove.has(it)) res.push(it); }\n" +
		"  return res;\n" +
		"}\n"

	helperIntersect = "function _intersect<T>(a: T[], b: T[]): T[] {\n" +
		"  const keep = new Set<T>(b);\n" +
		"  const res: T[] = [];\n" +
		"  const seen = new Set<T>();\n" +
		"  for (const it of a) { if (keep.has(it) && !seen.has(it)) { seen.add(it); res.push(it); } }\n" +
		"  return res;\n" +
		"}\n"

	helperStream = "class Stream {\n" +
		"  name: string;\n" +
		"  handlers: Array<(data: any) => any | Promise<any>> = [];\n" +
		"  constructor(name: string) {\n" +
		"    this.name = name;\n" +
		"  }\n" +
		"  append(data: any): Promise<any> {\n" +
		"    const tasks: Promise<any>[] = [];\n" +
		"    for (const h of [...this.handlers]) {\n" +
		"      tasks.push(Promise.resolve(h(data)));\n" +
		"    }\n" +
		"    const p = Promise.all(tasks).then(() => data);\n" +
		"    _pending.push(p);\n" +
		"    return p;\n" +
		"  }\n" +
		"  register(handler: (data: any) => any | Promise<any>): void {\n" +
		"    this.handlers.push(handler);\n" +
		"  }\n" +
		"}\n"

	helperWaitAll = "const _pending: Promise<any>[] = [];\n" +
		"async function _waitAll(): Promise<void> {\n" +
		"  await Promise.all(_pending);\n" +
		"}\n"

	helperAgent = "class Agent {\n" +
		"  name: string;\n" +
		"  handlers: { [key: string]: (ev: any) => any | Promise<any> } = {};\n" +
		"  intents: { [key: string]: (...args: any[]) => any } = {};\n" +
		"  state: { [key: string]: any } = {};\n" +
		"  constructor(name: string) {\n" +
		"    this.name = name;\n" +
		"  }\n" +
		"  start(): void {}\n" +
		"  on(stream: Stream, handler: (ev: any) => any | Promise<any>): void {\n" +
		"    stream.register(handler);\n" +
		"  }\n" +
		"  registerIntent(name: string, handler: (...args: any[]) => any): void {\n" +
		"    this.intents[name] = handler;\n" +
		"  }\n" +
		"  async call(name: string, ...args: any[]): Promise<any> {\n" +
		"    const fn = this.intents[name];\n" +
		"    if (!fn) throw new Error('unknown intent: ' + name);\n" +
		"    let res = fn(...args);\n" +
		"    if (res instanceof Promise) res = await res;\n" +
		"    return res;\n" +
		"  }\n" +
		"  set(name: string, value: any): void { this.state[name] = value; }\n" +
		"  get(name: string): any { return this.state[name]; }\n" +
		"}\n"

	helperQuery = "function _query(src: any[], joins: any[], opts: any): any {\n" +
		"  let items = src.map(v => [v]);\n" +
		"  for (const j of joins) {\n" +
		"    const joined: any[] = [];\n" +
		"    if (j.right && j.left) {\n" +
		"      const matched: boolean[] = new Array(j.items.length).fill(false);\n" +
		"      for (const left of items) {\n" +
		"        let m = false;\n" +
		"        for (let ri = 0; ri < j.items.length; ri++) {\n" +
		"          const right = j.items[ri];\n" +
		"          let keep = true;\n" +
		"          if (right === null) {\n" +
		"            keep = false;\n" +
		"          } else if (j.on) { keep = j.on(...left, right); }\n" +
		"          if (!keep) continue;\n" +
		"          m = true; matched[ri] = true;\n" +
		"          joined.push([...left, right]);\n" +
		"        }\n" +
		"        if (!m) joined.push([...left, null]);\n" +
		"      }\n" +
		"      for (let ri = 0; ri < j.items.length; ri++) {\n" +
		"        if (!matched[ri]) {\n" +
		"          const undef = Array(items[0]?.length || 0).fill(null);\n" +
		"          joined.push([...undef, j.items[ri]]);\n" +
		"        }\n" +
		"      }\n" +
		"    } else if (j.right) {\n" +
		"      for (const right of j.items) {\n" +
		"        let m = false;\n" +
		"        for (const left of items) {\n" +
		"          let keep = true;\n" +
		"          if (right === null) {\n" +
		"            keep = false;\n" +
		"          } else if (j.on) { keep = j.on(...left, right); }\n" +
		"          if (!keep) continue;\n" +
		"          m = true; joined.push([...left, right]);\n" +
		"        }\n" +
		"        if (!m) {\n" +
		"          const undef = Array(items[0]?.length || 0).fill(null);\n" +
		"          joined.push([...undef, right]);\n" +
		"        }\n" +
		"      }\n" +
		"    } else {\n" +
		"      for (const left of items) {\n" +
		"        let m = false;\n" +
		"        for (const right of j.items) {\n" +
		"          let keep = true;\n" +
		"          if (right === null) {\n" +
		"            keep = false;\n" +
		"          } else if (j.on) { keep = j.on(...left, right); }\n" +
		"          if (!keep) continue;\n" +
		"          m = true; joined.push([...left, right]);\n" +
		"        }\n" +
		"        if (j.left && !m) joined.push([...left, null]);\n" +
		"      }\n" +
		"    }\n" +
		"    items = joined;\n" +
		"  }\n" +
		"  if (opts.where) items = items.filter(r => opts.where(...r));\n" +
		"  if (opts.sortKey) {\n" +
		"    let pairs = items.map(it => ({item: it, key: opts.sortKey(...it)}));\n" +
		"    pairs.sort((a,b) => {\n" +
		"      const ak = a.key; const bk = b.key;\n" +
		"      if (typeof ak === 'number' && typeof bk === 'number') return ak - bk;\n" +
		"      if (typeof ak === 'string' && typeof bk === 'string') return ak < bk ? -1 : (ak > bk ? 1 : 0);\n" +
		"      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);\n" +
		"    });\n" +
		"    items = pairs.map(p => p.item);\n" +
		"  }\n" +
		"  if (opts.skip !== undefined) { const n = opts.skip; items = n < items.length ? items.slice(n) : []; }\n" +
		"  if (opts.take !== undefined) { const n = opts.take; if (n < items.length) items = items.slice(0, n); }\n" +
		"  const res = [];\n" +
		"  for (const r of items) res.push(opts.select(...r));\n" +
		"  return res;\n" +
		"}\n"

	helperHashJoin = "function _hashJoin(left: any[], right: any[], lk: (v: any) => any, rk: (v: any) => any): any[] {\n" +
		"  const idx = new Map<any, any[]>();\n" +
		"  for (const r of right) {\n" +
		"    const k = rk(r);\n" +
		"    const arr = idx.get(k);\n" +
		"    if (arr) arr.push(r); else idx.set(k, [r]);\n" +
		"  }\n" +
		"  const out: any[] = [];\n" +
		"  for (const l of left) {\n" +
		"    const arr = idx.get(lk(l));\n" +
		"    if (!arr) continue;\n" +
		"    for (const r of arr) out.push([l, r]);\n" +
		"  }\n" +
		"  return out;\n" +
		"}\n"

	helperDataset = "import { readAllSync } from \"https://deno.land/std@0.221.0/io/read_all.ts\";\n" +
		"import { parse as _yamlParse, stringify as _yamlStringify } from \"https://deno.land/std@0.221.0/yaml/mod.ts\";\n" +
		"function _readInput(path: string | null): string {\n" +
		"  if (!path || path === '-') {\n" +
		"    const data = readAllSync(Deno.stdin);\n" +
		"    return new TextDecoder().decode(data);\n" +
		"  }\n" +
		"  return Deno.readTextFileSync(path);\n" +
		"}\n" +
		"function _writeOutput(path: string | null, text: string): void {\n" +
		"  const data = new TextEncoder().encode(text);\n" +
		"  if (!path || path === '-') {\n" +
		"    Deno.stdout.writeSync(data);\n" +
		"  } else {\n" +
		"    Deno.writeFileSync(path, data);\n" +
		"  }\n" +
		"}\n" +
		"function _parseCSV(text: string, header: boolean, delim: string): any[] {\n" +
		"  const lines = text.trim().split(/\\r?\\n/);\n" +
		"  if (lines.length === 0) return [];\n" +
		"  let headers: string[] = [];\n" +
		"  let start = 0;\n" +
		"  if (header) { headers = lines[0].split(delim); start = 1; } else { headers = lines[0].split(delim).map((_,i)=>`c${i}`); }\n" +
		"  const out: any[] = [];\n" +
		"  for (let i=start; i<lines.length; i++) {\n" +
		"    if (!lines[i]) continue;\n" +
		"    const parts = lines[i].split(delim);\n" +
		"    const m: { [key: string]: any } = {};\n" +
		"    for (let j=0; j<headers.length; j++) {\n" +
		"      const val = parts[j] ?? '';\n" +
		"      if (/^-?\\d+$/.test(val)) m[headers[j]] = parseInt(val,10);\n" +
		"      else if (/^-?\\d*\\.\\d+(e[+-]?\\d+)?$/i.test(val)) m[headers[j]] = parseFloat(val);\n" +
		"      else m[headers[j]] = val;\n" +
		"    }\n" +
		"    out.push(m);\n" +
		"  }\n" +
		"  return out;\n" +
		"}\n" +
		"function _load(path: string | null, opts: any): any[] {\n" +
		"  const format = opts?.format ?? 'csv';\n" +
		"  const header = opts?.header ?? true;\n" +
		"  let delim = (opts?.delimiter ?? ',')[0];\n" +
		"  const text = _readInput(path);\n" +
		"  let items: any[];\n" +
		"  switch (format) {\n" +
		"    case 'jsonl':\n" +
		"      items = text.trim().split(/\\r?\\n/).filter(l=>l).map(l=>JSON.parse(l));\n" +
		"      break;\n" +
		"    case 'json':\n" +
		"      const d = JSON.parse(text); items = Array.isArray(d)?d:[d];\n" +
		"      break;\n" +
		"    case 'yaml':\n" +
		"      const y = _yamlParse(text); items = Array.isArray(y)?y:[y];\n" +
		"      break;\n" +
		"    case 'tsv':\n" +
		"      delim = '\t';\n" +
		"      items = _parseCSV(text, header, delim);\n" +
		"      break;\n" +
		"    default:\n" +
		"      items = _parseCSV(text, header, delim);\n" +
		"  }\n" +
		"  if (opts?.filter) {\n" +
		"    const f = _toAnyMap(opts.filter);\n" +
		"    items = items.filter(r => {\n" +
		"      for (const [k, v] of Object.entries(f)) {\n" +
		"        if (r[k] !== v) return false;\n" +
		"      }\n" +
		"      return true;\n" +
		"    });\n" +
		"  }\n" +
		"  if (opts?.page !== undefined && opts?.pageSize !== undefined) {\n" +
		"    const start = opts.page * opts.pageSize;\n" +
		"    items = start < items.length ? items.slice(start, start + opts.pageSize) : [];\n" +
		"  } else {\n" +
		"    if (opts?.skip !== undefined) { const n = opts.skip; items = n < items.length ? items.slice(n) : []; }\n" +
		"    if (opts?.take !== undefined) { const n = opts.take; if (n < items.length) items = items.slice(0, n); }\n" +
		"  }\n" +
		"  return items;\n" +
		"}\n" +
		"function _save(rows: any[], path: string | null, opts: any): void {\n" +
		"  const format = opts?.format ?? 'csv';\n" +
		"  const header = opts?.header ?? false;\n" +
		"  let delim = (opts?.delimiter ?? ',')[0];\n" +
		"  switch (format) {\n" +
		"    case 'jsonl':\n" +
		"      _writeOutput(path, rows.map(r=>JSON.stringify(r)).join('\\n') + '\\n');\n" +
		"      break;\n" +
		"    case 'json':\n" +
		"      _writeOutput(path, rows.length===1 ? JSON.stringify(rows[0]) : JSON.stringify(rows));\n" +
		"      break;\n" +
		"    case 'yaml':\n" +
		"      _writeOutput(path, rows.length===1 ? _yamlStringify(rows[0]) : _yamlStringify(rows));\n" +
		"      break;\n" +
		"    case 'tsv':\n" +
		"      delim = '\t';\n" +
		"    default:\n" +
		"      const headers = rows.length > 0 ? Object.keys(rows[0]).sort() : [];\n" +
		"      const lines: string[] = [];\n" +
		"      if (header) lines.push(headers.join(delim));\n" +
		"      for (const row of rows) {\n" +
		"        lines.push(headers.map(h => row[h] !== undefined ? String(row[h]) : '').join(delim));\n" +
		"      }\n" +
		"      _writeOutput(path, lines.join('\\n') + '\\n');\n" +
		"  }\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_indexString": helperIndexString,
	"_sliceString": helperSliceString,
	"_slice":       helperSlice,
	"_append":      helperAppend,
	"_count":       helperCount,
	"_avg":         helperAvg,
	"_reduce":      helperReduce,
	"_sum":         helperSum,
	"_min":         helperMin,
	"_max":         helperMax,
	"_exists":      helperExists,
	"_contains":    helperContains,
	"_starts_with": helperStartsWith,
	"_values":      helperValues,
	"_input":       helperInput,
	"_iter":        helperIter,
	"_now":         helperNow,
	"_gen_text":    helperGenText,
	"_gen_embed":   helperGenEmbed,
	"_gen_struct":  helperGenStruct,
	"_cmp":         helperCmp,
        "_equal":       helperEqual,
        "_fetch":       helperFetch,
       "_lookupHost":  helperLookupHost,
        "_toAnyMap":    helperToAnyMap,
	"_union_all":   helperUnionAll,
	"_union":       helperUnion,
	"_except":      helperExcept,
	"_intersect":   helperIntersect,
	"_stream":      helperStream,
	"_waitAll":     helperWaitAll,
	"_agent":       helperAgent,
	"_query":       helperQuery,
	"_hashJoin":    helperHashJoin,
	"_dataset":     helperDataset,
	"_json":        helperJSON,
	"_fmt":         helperFmt,
}

func (c *Compiler) use(name string) {
	c.helpers[name] = true
}

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
		c.buf.WriteByte('\n')
	}
}
