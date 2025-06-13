package pycode

import "sort"

// Runtime helpers emitted by the Python compiler.

var helperGenText = "def _gen_text(prompt, model=None, params=None):\n" +
	"    # TODO: send prompt to your LLM of choice\n" +
	"    return prompt\n"

var helperGenEmbed = "def _gen_embed(text, model=None, params=None):\n" +
	"    # TODO: send text to your embedding model of choice\n" +
	"    return [float(ord(c)) for c in text]\n"

var helperGenStruct = "def _gen_struct(cls, prompt, model=None, params=None):\n" +
	"    # TODO: send prompt to your LLM of choice and parse JSON\n" +
	"    import json\n" +
	"    data = json.loads(prompt)\n" +
	"    return cls(**data)\n"

var helperCount = "def _count(v):\n" +
	"    if isinstance(v, list):\n" +
	"        return len(v)\n" +
	"    if hasattr(v, 'Items'):\n" +
	"        return len(v.Items)\n" +
	"    raise Exception('count() expects list or group')\n"

var helperAvg = "def _avg(v):\n" +
	"    if hasattr(v, 'Items'):\n" +
	"        v = v.Items\n" +
	"    if not isinstance(v, list):\n" +
	"        raise Exception('avg() expects list or group')\n" +
	"    if not v:\n" +
	"        return 0\n" +
	"    s = 0.0\n" +
	"    for it in v:\n" +
	"        if isinstance(it, (int, float)):\n" +
	"            s += float(it)\n" +
	"        else:\n" +
	"            raise Exception('avg() expects numbers')\n" +
	"    return s / len(v)\n"

var helperFetch = "def _fetch(url, opts):\n" +
	"    import urllib.request, urllib.parse, json\n" +
	"    method = 'GET'\n" +
	"    data = None\n" +
	"    headers = {}\n" +
	"    timeout = None\n" +
	"    if opts:\n" +
	"        method = opts.get('method', method)\n" +
	"        if 'body' in opts:\n" +
	"            data = json.dumps(opts['body']).encode()\n" +
	"        if 'headers' in opts:\n" +
	"            for k, v in _to_any_map(opts['headers']).items():\n" +
	"                headers[k] = str(v)\n" +
	"        if 'query' in opts:\n" +
	"            q = urllib.parse.urlencode({k: str(v) for k, v in _to_any_map(opts['query']).items()})\n" +
	"            sep = '&' if '?' in url else '?'\n" +
	"            url = url + sep + q\n" +
	"        timeout = opts.get('timeout', None)\n" +
	"    req = urllib.request.Request(url, data=data, headers=headers, method=method)\n" +
	"    with urllib.request.urlopen(req, timeout=timeout) as resp:\n" +
	"        text = resp.read()\n" +
	"    return json.loads(text)\n"

var helperLoad = "def _load(path, opts):\n" +
	"    import csv, json, sys\n" +
	"    fmt = 'csv'\n" +
	"    header = True\n" +
	"    delim = ','\n" +
	"    if opts:\n" +
	"        fmt = opts.get('format', fmt)\n" +
	"        header = opts.get('header', header)\n" +
	"        delim = opts.get('delimiter', delim)\n" +
	"        if isinstance(delim, str) and delim:\n" +
	"            delim = delim[0]\n" +
	"    f = sys.stdin if path is None else open(path, 'r')\n" +
	"    try:\n" +
	"        if fmt == 'tsv':\n" +
	"            delim = '\t'; fmt = 'csv'\n" +
	"        if fmt == 'csv':\n" +
	"            rows = list(csv.reader(f, delimiter=delim))\n" +
	"            if not rows:\n" +
	"                return []\n" +
	"            if header:\n" +
	"                headers = rows[0]; rows = rows[1:]\n" +
	"            else:\n" +
	"                m = max(len(r) for r in rows)\n" +
	"                headers = [f'c{i}' for i in range(m)]\n" +
	"            out = []\n" +
	"            for rec in rows:\n" +
	"                row = {}\n" +
	"                for i, h in enumerate(headers):\n" +
	"                    val = rec[i] if i < len(rec) else ''\n" +
	"                    if val.isdigit():\n" +
	"                        row[h] = int(val)\n" +
	"                    else:\n" +
	"                        try:\n" +
	"                            row[h] = float(val)\n" +
	"                        except:\n" +
	"                            row[h] = val\n" +
	"                out.append(row)\n" +
	"            return out\n" +
	"        elif fmt == 'json':\n" +
	"            data = json.load(f)\n" +
	"            if isinstance(data, list):\n" +
	"                return [dict(d) for d in data]\n" +
	"            if isinstance(data, dict):\n" +
	"                return [dict(data)]\n" +
	"            return []\n" +
	"        elif fmt == 'jsonl':\n" +
	"            return [json.loads(line) for line in f if line.strip()]\n" +
	"        elif fmt == 'yaml':\n" +
	"            import yaml\n" +
	"            data = yaml.safe_load(f)\n" +
	"            if isinstance(data, list):\n" +
	"                return [dict(d) for d in data]\n" +
	"            if isinstance(data, dict):\n" +
	"                return [dict(data)]\n" +
	"            return []\n" +
	"        else:\n" +
	"            raise Exception('unknown format: ' + fmt)\n" +
	"    finally:\n" +
	"        if path is not None:\n" +
	"            f.close()\n"

var helperSave = "def _save(rows, path, opts):\n" +
	"    import csv, json, sys\n" +
	"    fmt = 'csv'\n" +
	"    header = False\n" +
	"    delim = ','\n" +
	"    if opts:\n" +
	"        fmt = opts.get('format', fmt)\n" +
	"        header = opts.get('header', header)\n" +
	"        delim = opts.get('delimiter', delim)\n" +
	"        if isinstance(delim, str) and delim:\n" +
	"            delim = delim[0]\n" +
	"    f = sys.stdout if path is None else open(path, 'w')\n" +
	"    try:\n" +
	"        if fmt == 'tsv':\n" +
	"            delim = '\t'; fmt = 'csv'\n" +
	"        if fmt == 'csv':\n" +
	"            w = csv.writer(f, delimiter=delim)\n" +
	"            headers = sorted(rows[0].keys()) if rows else []\n" +
	"            if header:\n" +
	"                w.writerow(headers)\n" +
	"            for row in rows:\n" +
	"                rec = []\n" +
	"                for h in headers:\n" +
	"                    val = row.get(h)\n" +
	"                    if isinstance(val, (dict, list)):\n" +
	"                        rec.append(json.dumps(val))\n" +
	"                    elif val is None:\n" +
	"                        rec.append('')\n" +
	"                    else:\n" +
	"                        rec.append(str(val))\n" +
	"                w.writerow(rec)\n" +
	"            return\n" +
	"        elif fmt == 'json':\n" +
	"            json.dump(rows, f)\n" +
	"        elif fmt == 'jsonl':\n" +
	"            for row in rows:\n" +
	"                f.write(json.dumps(row))\n" +
	"                f.write('\\n')\n" +
	"        elif fmt == 'yaml':\n" +
	"            import yaml\n" +
	"            yaml.safe_dump(rows[0] if len(rows) == 1 else rows, f)\n" +
	"        else:\n" +
	"            raise Exception('unknown format: ' + fmt)\n" +
	"    finally:\n" +
	"        if path is not None:\n" +
	"            f.close()\n"

var helperToAnyMap = "def _to_any_map(m):\n" +
	"    return dict(m) if isinstance(m, dict) else dict(m)\n"

var helperUnionAll = "def _union_all(a, b):\n" +
	"    return list(a) + list(b)\n"

var helperUnion = "def _union(a, b):\n" +
	"    res = list(a)\n" +
	"    for it in b:\n" +
	"        if it not in res:\n" +
	"            res.append(it)\n" +
	"    return res\n"

var helperExcept = "def _except(a, b):\n" +
	"    res = []\n" +
	"    for it in a:\n" +
	"        if it not in b:\n" +
	"            res.append(it)\n" +
	"    return res\n"

var helperIntersect = "def _intersect(a, b):\n" +
	"    res = []\n" +
	"    for it in a:\n" +
	"        if it in b and it not in res:\n" +
	"            res.append(it)\n" +
	"    return res\n"

var helperStream = "class Stream:\n" +
	"    def __init__(self, name):\n" +
	"        self.name = name\n" +
	"        self.handlers = []\n" +
	"    def append(self, data):\n" +
	"        tasks = []\n" +
	"        for h in list(self.handlers):\n" +
	"            r = h(data)\n" +
	"            if asyncio.iscoroutine(r):\n" +
	"                tasks.append(asyncio.create_task(r))\n" +
	"        async def _wait():\n" +
	"            if tasks:\n" +
	"                await asyncio.gather(*tasks)\n" +
	"            return data\n" +
	"        p = asyncio.create_task(_wait())\n" +
	"        _pending.append(p)\n" +
	"        return p\n" +
	"    def register(self, handler):\n" +
	"        self.handlers.append(handler)\n"

var helperWaitAll = "import asyncio\n" +
	"_pending = []\n" +
	"async def _wait_all():\n" +
	"    await asyncio.gather(*_pending)\n"

var helperAgent = "import asyncio\n" +
	"class Agent:\n" +
	"    def __init__(self, name):\n" +
	"        self.name = name\n" +
	"        self.handlers = {}\n" +
	"        self.intents = {}\n" +
	"        self.state = {}\n" +
	"    def start(self):\n" +
	"        pass\n" +
	"    def on(self, stream, handler):\n" +
	"        stream.register(handler)\n" +
	"    def register_intent(self, name, handler):\n" +
	"        self.intents[name] = handler\n" +
	"    async def call(self, name, *args):\n" +
	"        fn = self.intents.get(name)\n" +
	"        if fn is None:\n" +
	"            raise Exception('unknown intent: ' + name)\n" +
	"        res = fn(*args)\n" +
	"        if asyncio.iscoroutine(res):\n" +
	"            res = await res\n" +
	"        return res\n" +
	"    def set(self, name, value):\n" +
	"        self.state[name] = value\n" +
	"    def get(self, name):\n" +
	"        return self.state.get(name)\n"

var helperQuery = "def _query(src, joins, opts):\n" +
	"    items = [[v] for v in src]\n" +
	"    for j in joins:\n" +
	"        joined = []\n" +
	"        if j.get('right') and j.get('left'):\n" +
	"            matched = [False] * len(j['items'])\n" +
	"            for left in items:\n" +
	"                m = False\n" +
	"                for ri, right in enumerate(j['items']):\n" +
	"                    keep = True\n" +
	"                    if j.get('on'):\n" +
	"                        keep = j['on'](*left, right)\n" +
	"                    if not keep:\n" +
	"                        continue\n" +
	"                    m = True; matched[ri] = True\n" +
	"                    joined.append(left + [right])\n" +
	"                if not m:\n" +
	"                    joined.append(left + [None])\n" +
	"            for ri, right in enumerate(j['items']):\n" +
	"                if not matched[ri]:\n" +
	"                    undef = [None] * (len(items[0]) if items else 0)\n" +
	"                    joined.append(undef + [right])\n" +
	"        elif j.get('right'):\n" +
	"            for right in j['items']:\n" +
	"                m = False\n" +
	"                for left in items:\n" +
	"                    keep = True\n" +
	"                    if j.get('on'):\n" +
	"                        keep = j['on'](*left, right)\n" +
	"                    if not keep:\n" +
	"                        continue\n" +
	"                    m = True; joined.append(left + [right])\n" +
	"                if not m:\n" +
	"                    undef = [None] * (len(items[0]) if items else 0)\n" +
	"                    joined.append(undef + [right])\n" +
	"        else:\n" +
	"            for left in items:\n" +
	"                m = False\n" +
	"                for right in j['items']:\n" +
	"                    keep = True\n" +
	"                    if j.get('on'):\n" +
	"                        keep = j['on'](*left, right)\n" +
	"                    if not keep:\n" +
	"                        continue\n" +
	"                    m = True; joined.append(left + [right])\n" +
	"                if j.get('left') and not m:\n" +
	"                    joined.append(left + [None])\n" +
	"        items = joined\n" +
	"    if opts.get('where'):\n" +
	"        items = [r for r in items if opts['where'](*r)]\n" +
	"    if opts.get('sortKey'):\n" +
	"        items.sort(key=lambda it: opts['sortKey'](*it))\n" +
	"    if 'skip' in opts:\n" +
	"        n = opts['skip']; items = items[n:] if n < len(items) else []\n" +
	"    if 'take' in opts:\n" +
	"        n = opts['take']; items = items[:n] if n < len(items) else items\n" +
	"    res = []\n" +
	"    for r in items:\n" +
	"        res.append(opts['select'](*r))\n" +
	"    return res\n"

var helperMap = map[string]string{
	"_gen_text":   helperGenText,
	"_gen_embed":  helperGenEmbed,
	"_gen_struct": helperGenStruct,
	"_count":      helperCount,
	"_avg":        helperAvg,
	"_union_all":  helperUnionAll,
	"_union":      helperUnion,
	"_except":     helperExcept,
	"_intersect":  helperIntersect,
	"_fetch":      helperFetch,
	"_load":       helperLoad,
	"_save":       helperSave,
	"_to_any_map": helperToAnyMap,
	"_stream":     helperStream,
	"_wait_all":   helperWaitAll,
	"_agent":      helperAgent,
	"_query":      helperQuery,
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitRuntime() {
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
	}
}
