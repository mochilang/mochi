//go:build slow

package pycode

import "sort"

var helperPrelude = "from typing import Any, TypeVar, Generic, Callable\nT = TypeVar('T')\nK = TypeVar('K')\n"

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

var helperExists = "def _exists(v):\n" +
	"    if hasattr(v, 'Items'):\n" +
	"        v = v.Items\n" +
	"    if isinstance(v, (list, dict, str)):\n" +
	"        return len(v) > 0\n" +
	"    raise Exception('exists expects list, map or string')\n"

var helperSum = "def _sum(v):\n" +
	"    if hasattr(v, 'Items'):\n" +
	"        v = v.Items\n" +
	"    if not isinstance(v, list):\n" +
	"        raise Exception('sum() expects list or group')\n" +
	"    s = 0.0\n" +
	"    for it in v:\n" +
	"        if it is None:\n" +
	"            continue\n" +
	"        if isinstance(it, (int, float)):\n" +
	"            s += float(it)\n" +
	"        else:\n" +
	"            raise Exception('sum() expects numbers')\n" +
	"    return s\n"

var helperMin = "def _min(v):\n" +
	"    if hasattr(v, 'Items'):\n" +
	"        v = v.Items\n" +
	"    if not isinstance(v, list):\n" +
	"        raise Exception('min() expects list or group')\n" +
	"    vals = [it for it in v if it is not None]\n" +
	"    if not vals:\n" +
	"        return 0\n" +
	"    return min(vals)\n"

var helperMax = "def _max(v):\n" +
	"    if hasattr(v, 'Items'):\n" +
	"        v = v.Items\n" +
	"    if not isinstance(v, list):\n" +
	"        raise Exception('max() expects list or group')\n" +
	"    vals = [it for it in v if it is not None]\n" +
	"    if not vals:\n" +
	"        return 0\n" +
	"    return max(vals)\n"

var helperGroupClass = "class _Group(Generic[K, T]):\n" +
	"    def __init__(self, key: K):\n" +
	"        self.key = key\n" +
	"        self.Items: list[T] = []\n" +
	"        self.items = self.Items\n" +
	"    def __iter__(self):\n" +
	"        return iter(self.Items)\n"

var helperGroupBy = "def _group_by(src: list[T], keyfn: Callable[[T], K]) -> list[_Group[K, T]]:\n" +
	"    groups: dict[str, _Group[K, T]] = {}\n" +
	"    order: list[str] = []\n" +
	"    for it in src:\n" +
	"        if isinstance(it, (list, tuple)):\n" +
	"            key = keyfn(*it)\n" +
	"        else:\n" +
	"            key = keyfn(it)\n" +
	"        if isinstance(key, dict):\n" +
	"            import types\n" +
	"            key = types.SimpleNamespace(**key)\n" +
	"        ks = str(key)\n" +
	"        g = groups.get(ks)\n" +
	"        if not g:\n" +
	"            g = _Group(key)\n" +
	"            groups[ks] = g\n" +
	"            order.append(ks)\n" +
	"        g.Items.append(it)\n" +
	"    return [ groups[k] for k in order ]\n"

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
	"            for k, v in dict(opts['headers']).items():\n" +
	"                headers[k] = str(v)\n" +
	"        if 'query' in opts:\n" +
	"            q = urllib.parse.urlencode({k: str(v) for k, v in dict(opts['query']).items()})\n" +
	"            sep = '&' if '?' in url else '?'\n" +
	"            url = url + sep + q\n" +
	"        timeout = opts.get('timeout', None)\n" +
	"    req = urllib.request.Request(url, data=data, headers=headers, method=method)\n" +
	"    with urllib.request.urlopen(req, timeout=timeout) as resp:\n" +
	"        text = resp.read()\n" +
	"    return json.loads(text)\n"

var helperLoad = "def _load(path, opts):\n" +
	"    import csv, json, sys, os\n" +
	"    fmt = 'csv'\n" +
	"    header = True\n" +
	"    delim = ','\n" +
	"    if opts:\n" +
	"        fmt = opts.get('format', fmt)\n" +
	"        header = opts.get('header', header)\n" +
	"        delim = opts.get('delimiter', delim)\n" +
	"        if isinstance(delim, str) and delim:\n" +
	"            delim = delim[0]\n" +
	"    if path is not None and not os.path.isabs(path):\n" +
	"        base = os.path.join(os.path.dirname(__file__), path)\n" +
	"        if os.path.exists(base):\n" +
	"            path = base\n" +
	"        elif os.environ.get('MOCHI_ROOT'):\n" +
	"            root = os.environ.get('MOCHI_ROOT')\n" +
	"            clean = path\n" +
	"            while clean.startswith('../'):\n" +
	"                clean = clean[3:]\n" +
	"            p = os.path.join(root, 'tests', clean)\n" +
	"            if not os.path.exists(p):\n" +
	"                p = os.path.join(root, clean)\n" +
	"            path = os.path.normpath(p)\n" +
	"    f = sys.stdin if path is None or path == '-' else open(path, 'r')\n" +
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
	"            try:\n" +
	"                import yaml\n" +
	"                data = yaml.safe_load(f)\n" +
	"            except Exception:\n" +
	"                data = []\n" +
	"                cur = None\n" +
	"                for line in f:\n" +
	"                    line = line.rstrip()\n" +
	"                    if not line:\n" +
	"                        continue\n" +
	"                    if line.startswith('- '):\n" +
	"                        if cur is not None:\n" +
	"                            data.append(cur)\n" +
	"                        cur = {}\n" +
	"                        line = line[2:]\n" +
	"                    if ':' in line:\n" +
	"                        k, v = line.split(':', 1)\n" +
	"                        k = k.strip()\n" +
	"                        v = v.strip()\n" +
	"                        if v.isdigit():\n" +
	"                            cur[k] = int(v)\n" +
	"                        else:\n" +
	"                            try:\n" +
	"                                cur[k] = float(v)\n" +
	"                            except:\n" +
	"                                cur[k] = v\n" +
	"                if cur is not None:\n" +
	"                    data.append(cur)\n" +
	"            if isinstance(data, list):\n" +
	"                return [dict(d) for d in data]\n" +
	"            if isinstance(data, dict):\n" +
	"                return [dict(data)]\n" +
	"            return []\n" +
	"        else:\n" +
	"            raise Exception('unknown format: ' + fmt)\n" +
	"    finally:\n" +
	"        if path is not None and path != '-':\n" +
	"            f.close()\n"

var helperSave = "def _save(rows, path, opts):\n" +
	"    import csv, json, sys, dataclasses, os\n" +
	"    fmt = 'csv'\n" +
	"    header = False\n" +
	"    delim = ','\n" +
	"    if opts:\n" +
	"        fmt = opts.get('format', fmt)\n" +
	"        header = opts.get('header', header)\n" +
	"        delim = opts.get('delimiter', delim)\n" +
	"        if isinstance(delim, str) and delim:\n" +
	"            delim = delim[0]\n" +
	"    rows = [ dataclasses.asdict(r) if dataclasses.is_dataclass(r) else r for r in rows ]\n" +
	"    if path is not None and path != '-' and not os.path.isabs(path):\n" +
	"        base = os.path.join(os.path.dirname(__file__), path)\n" +
	"        if not os.path.exists(base) and os.environ.get('MOCHI_ROOT'):\n" +
	"            clean = path\n" +
	"            while clean.startswith('../'):\n" +
	"                clean = clean[3:]\n" +
	"            base = os.path.join(os.environ.get('MOCHI_ROOT'), clean)\n" +
	"        path = base\n" +
	"    f = sys.stdout if path is None or path == '-' else open(path, 'w')\n" +
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
	"        if path is not None and path != '-':\n" +
	"            f.close()\n"

var helperSlice = "def _slice(obj: list[T] | str, i: int, j: int) -> list[T] | str:\n" +
	"    start = i\n" +
	"    end = j\n" +
	"    n = len(obj)\n" +
	"    if start < 0:\n" +
	"        start += n\n" +
	"    if end < 0:\n" +
	"        end += n\n" +
	"    if start < 0:\n" +
	"        start = 0\n" +
	"    if end > n:\n" +
	"        end = n\n" +
	"    if end < start:\n" +
	"        end = start\n" +
	"    return obj[start:end]\n"

var helperReverse = "def _reverse(obj: list[T] | str) -> list[T] | str:\n" +
	"    if isinstance(obj, list):\n" +
	"        return list(reversed(obj))\n" +
	"    if isinstance(obj, str):\n" +
	"        return obj[::-1]\n" +
	"    raise Exception('reverse expects list or string')\n"

var helperFirst = "def _first(lst: list[T]) -> T | None:\n" +
	"    if not isinstance(lst, list):\n" +
	"        raise Exception('first expects list')\n" +
	"    return lst[0] if len(lst) > 0 else None\n"

var helperUnionAll = "def _union_all(a: list[T], b: list[T]) -> list[T]:\n" +
	"    return list(a) + list(b)\n"

var helperUnion = "def _union(a: list[T], b: list[T]) -> list[T]:\n" +
	"    res = list(a)\n" +
	"    for it in b:\n" +
	"        if it not in res:\n" +
	"            res.append(it)\n" +
	"    return res\n"

var helperExcept = "def _except(a: list[T], b: list[T]) -> list[T]:\n" +
	"    res = []\n" +
	"    for it in a:\n" +
	"        if it not in b:\n" +
	"            res.append(it)\n" +
	"    return res\n"

var helperIntersect = "def _intersect(a: list[T], b: list[T]) -> list[T]:\n" +
	"    res = []\n" +
	"    for it in a:\n" +
	"        if it in b and it not in res:\n" +
	"            res.append(it)\n" +
	"    return res\n"

var helperGet = "def _get(obj, name):\n" +
	"    if obj is None:\n" +
	"        return None\n" +
	"    if isinstance(obj, dict):\n" +
	"        if name in obj:\n" +
	"            return obj[name]\n" +
	"    if hasattr(obj, name):\n" +
	"        return getattr(obj, name)\n" +
	"    if name == 'items' and hasattr(obj, 'Items'):\n" +
	"        return getattr(obj, 'Items')\n" +
	"    if isinstance(obj, (list, tuple)):\n" +
	"        for it in obj:\n" +
	"            try:\n" +
	"                return _get(it, name)\n" +
	"            except Exception:\n" +
	"                pass\n" +
	"    raise Exception('field not found: ' + name)\n"

var helperSortKey = "def _sort_key(k):\n" +
	"    if isinstance(k, (list, tuple, dict)):\n" +
	"        return str(k)\n" +
	"    return k\n"

var helperAppend = "def _append(lst: list[T] | None, v: T) -> list[T]:\n" +
	"    out: list[T] = list(lst) if lst is not None else []\n" +
	"    out.append(v)\n" +
	"    return out\n"

var helperContains = "def _contains(c: list[T] | str | dict[str, T], v: T) -> bool:\n" +
	"    if isinstance(c, list):\n" +
	"        return v in c\n" +
	"    if isinstance(c, str):\n" +
	"        return str(v) in c\n" +
	"    if isinstance(c, dict):\n" +
	"        return str(v) in c\n" +
	"    return False\n"

var helperValues = "def _values(m: dict[str, T]) -> list[T]:\n" +
	"    if isinstance(m, dict):\n" +
	"        return list(m.values())\n" +
	"    raise Exception('values() expects map')\n"

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
	"        def _key(it):\n" +
	"            k = opts['sortKey'](*it)\n" +
	"            if isinstance(k, (list, tuple, dict)):\n" +
	"                return str(k)\n" +
	"            return k\n" +
	"        items.sort(key=_key)\n" +
	"    if 'skip' in opts:\n" +
	"        n = opts['skip']\n" +
	"        if n < 0:\n" +
	"            n = 0\n" +
	"        items = items[n:] if n < len(items) else []\n" +
	"    if 'take' in opts:\n" +
	"        n = opts['take']\n" +
	"        if n < 0:\n" +
	"            n = 0\n" +
	"        items = items[:n] if n < len(items) else items\n" +
	"    res = []\n" +
	"    for r in items:\n" +
	"        res.append(opts['select'](*r))\n" +
	"    return res\n"

var helperMap = map[string]string{
	"_gen_text":   helperGenText,
	"_gen_embed":  helperGenEmbed,
	"_gen_struct": helperGenStruct,
	"_group":      helperGroupClass,
	"_group_by":   helperGroupBy,
	"_count":      helperCount,
	"_avg":        helperAvg,
	"_exists":     helperExists,
	"_sum":        helperSum,
	"_min":        helperMin,
	"_max":        helperMax,
	"_first":      helperFirst,
	"_union_all":  helperUnionAll,
	"_union":      helperUnion,
	"_except":     helperExcept,
	"_intersect":  helperIntersect,
	"_get":        helperGet,
	"_append":     helperAppend,
	"_contains":   helperContains,
	"_values":     helperValues,
	"_fetch":      helperFetch,
	"_load":       helperLoad,
	"_save":       helperSave,
	"_slice":      helperSlice,
	"_reverse":    helperReverse,
	"_stream":     helperStream,
	"_wait_all":   helperWaitAll,
	"_agent":      helperAgent,
	"_sort_key":   helperSortKey,
	"_query":      helperQuery,
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitRuntime() {
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	if len(names) > 0 {
		c.buf.WriteString(helperPrelude)
	}
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
	}
}
