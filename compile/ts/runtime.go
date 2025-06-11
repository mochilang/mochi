package tscode

import "sort"

// Runtime helper functions injected into generated programs.
const (
	helperIndex = "function _index(v: any, k: any): any {\n" +
		"  if (Array.isArray(v)) {\n" +
		"    if (typeof k !== 'number') throw new Error('invalid list index');\n" +
		"    if (k < 0) k += v.length;\n" +
		"    if (k < 0 || k >= v.length) throw new Error('index out of range');\n" +
		"    return v[k];\n" +
		"  }\n" +
		"  if (typeof v === 'string') {\n" +
		"    if (typeof k !== 'number') throw new Error('invalid string index');\n" +
		"    const chars = Array.from(v);\n" +
		"    if (k < 0) k += chars.length;\n" +
		"    if (k < 0 || k >= chars.length) throw new Error('index out of range');\n" +
		"    return chars[k];\n" +
		"  }\n" +
		"  if (v && typeof v === 'object') {\n" +
		"    return (v as any)[k];\n" +
		"  }\n" +
		"  return (v as any)[k];\n" +
		"}\n"

	helperLen = "function _len(v: any): number {\n" +
		"  if (Array.isArray(v) || typeof v === \"string\") return (v as any).length;\n" +
		"  if (v && typeof v === \"object\") return Object.keys(v).length;\n" +
		"  return 0;\n" +
		"}\n"

	helperIter = "function _iter(v: any): any {\n" +
		"  if (v && typeof v === 'object' && !Array.isArray(v) && !(Symbol.iterator in v)) {\n" +
		"    return Object.keys(v);\n" +
		"  }\n" +
		"  return v;\n" +
		"}\n"

	helperGenText = "function _gen_text(prompt: string, model: string | null, params: any | null): string {\n" +
		"  // TODO: integrate with your preferred LLM\n" +
		"  return prompt;\n" +
		"}\n"

	helperGenEmbed = "function _gen_embed(text: string, model: string | null, params: any | null): number[] {\n" +
		"  // TODO: integrate with your preferred embedding model\n" +
		"  return Array.from(text).map(c => c.charCodeAt(0));\n" +
		"}\n"

	helperGenStruct = "function _gen_struct<T>(prompt: string, model: string | null, params: any | null): T {\n" +
		"  // TODO: integrate with your preferred LLM and parse JSON\n" +
		"  return JSON.parse(prompt) as T;\n" +
		"}\n"

	helperFetch = "function _fetch(url: string, opts: any): any {\n" +
		"  const args: string[] = ['-s'];\n" +
		"  const method = opts?.method ?? 'GET';\n" +
		"  args.push('-X', method);\n" +
		"  if (opts?.headers) {\n" +
		"    for (const [k, v] of Object.entries(_toAnyMap(opts.headers))) {\n" +
		"      args.push('-H', `${k}: ${String(v)}`);\n" +
		"    }\n" +
		"  }\n" +
		"  if (opts?.query) {\n" +
		"    const qs = new URLSearchParams();\n" +
		"    for (const [k, v] of Object.entries(_toAnyMap(opts.query))) {\n" +
		"      qs.set(k, String(v));\n" +
		"    }\n" +
		"    const sep = url.includes('?') ? '&' : '?';\n" +
		"    url = url + sep + qs.toString();\n" +
		"  }\n" +
		"  if (opts && 'body' in opts) {\n" +
		"    args.push('-d', JSON.stringify(opts.body));\n" +
		"  }\n" +
		"  if (opts?.timeout) {\n" +
		"    args.push('--max-time', String(opts.timeout));\n" +
		"  }\n" +
		"  args.push(url);\n" +
		"  const { stdout } = new Deno.Command('curl', { args }).outputSync();\n" +
		"  return JSON.parse(new TextDecoder().decode(stdout));\n" +
		"}\n"

	helperToAnyMap = "function _toAnyMap(m: any): Record<string, any> {\n" +
		"  return m as Record<string, any>;\n" +
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
		"  handlers: Record<string, (ev: any) => any | Promise<any>> = {};\n" +
		"  intents: Record<string, (...args: any[]) => any> = {};\n" +
		"  state: Record<string, any> = {};\n" +
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
		"          if (j.on) keep = j.on(...left, right);\n" +
		"          if (!keep) continue;\n" +
		"          m = true; matched[ri] = true;\n" +
		"          joined.push([...left, right]);\n" +
		"        }\n" +
		"        if (!m) joined.push([...left, undefined]);\n" +
		"      }\n" +
		"      for (let ri = 0; ri < j.items.length; ri++) {\n" +
		"        if (!matched[ri]) {\n" +
		"          const undef = Array(items[0]?.length || 0).fill(undefined);\n" +
		"          joined.push([...undef, j.items[ri]]);\n" +
		"        }\n" +
		"      }\n" +
		"    } else if (j.right) {\n" +
		"      for (const right of j.items) {\n" +
		"        let m = false;\n" +
		"        for (const left of items) {\n" +
		"          let keep = true;\n" +
		"          if (j.on) keep = j.on(...left, right);\n" +
		"          if (!keep) continue;\n" +
		"          m = true; joined.push([...left, right]);\n" +
		"        }\n" +
		"        if (!m) {\n" +
		"          const undef = Array(items[0]?.length || 0).fill(undefined);\n" +
		"          joined.push([...undef, right]);\n" +
		"        }\n" +
		"      }\n" +
		"    } else {\n" +
		"      for (const left of items) {\n" +
		"        let m = false;\n" +
		"        for (const right of j.items) {\n" +
		"          let keep = true;\n" +
		"          if (j.on) keep = j.on(...left, right);\n" +
		"          if (!keep) continue;\n" +
		"          m = true; joined.push([...left, right]);\n" +
		"        }\n" +
		"        if (j.left && !m) joined.push([...left, undefined]);\n" +
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
		"    const m: Record<string, any> = {};\n" +
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
		"  switch (format) {\n" +
		"    case 'jsonl':\n" +
		"      return text.trim().split(/\\r?\\n/).filter(l=>l).map(l=>JSON.parse(l));\n" +
		"    case 'json':\n" +
		"      const d = JSON.parse(text); return Array.isArray(d)?d:[d];\n" +
		"    case 'yaml':\n" +
		"      const y = _yamlParse(text); return Array.isArray(y)?y:[y];\n" +
		"    case 'tsv':\n" +
		"      delim = '\t';\n" +
		"      return _parseCSV(text, header, delim);\n" +
		"    default:\n" +
		"      return _parseCSV(text, header, delim);\n" +
		"  }\n" +
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
	"_index":      helperIndex,
	"_len":        helperLen,
	"_iter":       helperIter,
	"_gen_text":   helperGenText,
	"_gen_embed":  helperGenEmbed,
	"_gen_struct": helperGenStruct,
	"_fetch":      helperFetch,
	"_toAnyMap":   helperToAnyMap,
	"_stream":     helperStream,
	"_waitAll":    helperWaitAll,
	"_agent":      helperAgent,
	"_query":      helperQuery,
	"_dataset":    helperDataset,
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
