package pycode

import "sort"

// Runtime helpers emitted by the Python compiler.

var helperIndex = "def _index(v, k):\n" +
	"    if isinstance(v, list):\n" +
	"        if not isinstance(k, int):\n" +
	"            raise Exception('invalid list index')\n" +
	"        if k < 0:\n" +
	"            k += len(v)\n" +
	"        if k < 0 or k >= len(v):\n" +
	"            raise Exception('index out of range')\n" +
	"        return v[k]\n" +
	"    if isinstance(v, str):\n" +
	"        if not isinstance(k, int):\n" +
	"            raise Exception('invalid string index')\n" +
	"        if k < 0:\n" +
	"            k += len(v)\n" +
	"        if k < 0 or k >= len(v):\n" +
	"            raise Exception('index out of range')\n" +
	"        return v[k]\n" +
	"    if isinstance(v, dict):\n" +
	"        return v[k]\n" +
	"    return v[k]\n"

var helperGenText = "def _gen_text(prompt):\n" +
	"    # TODO: send prompt to your LLM of choice\n" +
	"    return prompt\n"

var helperGenEmbed = "def _gen_embed(text):\n" +
	"    # TODO: send text to your embedding model of choice\n" +
	"    return [float(ord(c)) for c in text]\n"

var helperGenStruct = "def _gen_struct(cls, prompt):\n" +
	"    # TODO: send prompt to your LLM of choice and parse JSON\n" +
	"    import json\n" +
	"    data = json.loads(prompt)\n" +
	"    return cls(**data)\n"

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

var helperToAnyMap = "def _to_any_map(m):\n" +
	"    return dict(m) if isinstance(m, dict) else dict(m)\n"

var helperIter = "def _iter(v):\n" +
	"    if isinstance(v, dict):\n" +
	"        return list(v.keys())\n" +
	"    return v\n"

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
	"_index":      helperIndex,
	"_gen_text":   helperGenText,
	"_gen_embed":  helperGenEmbed,
	"_gen_struct": helperGenStruct,
	"_fetch":      helperFetch,
	"_to_any_map": helperToAnyMap,
	"_iter":       helperIter,
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
