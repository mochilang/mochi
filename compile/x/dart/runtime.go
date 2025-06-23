package dartcode

const (
	helperIndexString = "String _indexString(String s, int i) {\n" +
		"    var runes = s.runes.toList();\n" +
		"    if (i < 0) {\n" +
		"        i += runes.length;\n" +
		"    }\n" +
		"    if (i < 0 || i >= runes.length) {\n" +
		"        throw RangeError('index out of range');\n" +
		"    }\n" +
		"    return String.fromCharCode(runes[i]);\n" +
		"}\n"
	helperUnionAll = "List<dynamic> _unionAll(List<dynamic> a, List<dynamic> b) => [...a, ...b];\n"
	helperUnion    = "List<dynamic> _union(List<dynamic> a, List<dynamic> b) {\n" +
		"    var res = [...a];\n" +
		"    for (var it in b) {\n" +
		"        if (!res.contains(it)) {\n" +
		"            res.add(it);\n" +
		"        }\n" +
		"    }\n" +
		"    return res;\n" +
		"}\n"
	helperExcept = "List<dynamic> _except(List<dynamic> a, List<dynamic> b) {\n" +
		"    var res = <dynamic>[];\n" +
		"    for (var it in a) {\n" +
		"        if (!b.contains(it)) {\n" +
		"            res.add(it);\n" +
		"        }\n" +
		"    }\n" +
		"    return res;\n" +
		"}\n"
	helperIntersect = "List<dynamic> _intersect(List<dynamic> a, List<dynamic> b) {\n" +
		"    var res = <dynamic>[];\n" +
		"    for (var it in a) {\n" +
		"        if (b.contains(it) && !res.contains(it)) {\n" +
		"            res.add(it);\n" +
		"        }\n" +
		"    }\n" +
		"    return res;\n" +
		"}\n"
	helperStream = "class _Stream<T> {\n" +
		"    String name;\n" +
		"    List<void Function(T)> handlers = [];\n" +
		"    _Stream(this.name);\n" +
		"    Future<T> append(T data) {\n" +
		"        var tasks = <Future<dynamic>>[];\n" +
		"        for (var h in List.from(handlers)) {\n" +
		"            var res = h(data);\n" +
		"            if (res is Future) tasks.add(res);\n" +
		"        }\n" +
		"        var f = Future.wait(tasks).then((_) => data);\n" +
		"        _pending.add(f);\n" +
		"        return f;\n" +
		"    }\n" +
		"    void register(void Function(T) handler) { handlers.add(handler); }\n" +
		"}\n"
	helperWaitAll = "List<Future<dynamic>> _pending = [];\n" +
		"Future<void> _waitAll() async {\n" +
		"    await Future.wait(_pending);\n" +
		"}\n"
	helperAgent = "class _Agent {\n" +
		"    String name;\n" +
		"    Map<String, Function> intents = {};\n" +
		"    Map<String, dynamic> state = {};\n" +
		"    _Agent(this.name);\n" +
		"    void start() {}\n" +
		"    void on(_Stream<dynamic> s, Function handler) { s.register((ev) { var res = handler(ev); if (res is Future) res.then((_){}); }); }\n" +
		"    void registerIntent(String name, Function handler) { intents[name] = handler; }\n" +
		"    Future<dynamic> call(String name, List<dynamic> args) async {\n" +
		"        var fn = intents[name];\n" +
		"        if (fn == null) throw Exception('unknown intent: $name');\n" +
		"        var res = Function.apply(fn, args);\n" +
		"        if (res is Future) res = await res;\n" +
		"        return res;\n" +
		"    }\n" +
		"    void set(String name, dynamic value) { state[name] = value; }\n" +
		"    dynamic get(String name) => state[name];\n" +
		"}\n"
	helperGroup = "class _Group {\n" +
		"    dynamic key;\n" +
		"    List<dynamic> Items = [];\n" +
		"    _Group(this.key);\n" +
		"}\n"
	helperGroupBy = "List<_Group> _group_by(List<dynamic> src, dynamic Function(dynamic) keyfn) {\n" +
		"    var groups = <String,_Group>{};\n" +
		"    var order = <String>[];\n" +
		"    for (var it in src) {\n" +
		"        var key = keyfn(it);\n" +
		"        var ks = key.toString();\n" +
		"        var g = groups[ks];\n" +
		"        if (g == null) {\n" +
		"            g = _Group(key);\n" +
		"            groups[ks] = g;\n" +
		"            order.add(ks);\n" +
		"        }\n" +
		"        g.Items.add(it);\n" +
		"    }\n" +
		"    return [for (var k in order) groups[k]!];\n" +
		"}\n"
	helperFetch = "dynamic _fetch(String url, Map<String,dynamic>? opts) {\n" +
		"    var args = ['-s'];\n" +
		"    var method = opts?['method']?.toString() ?? 'GET';\n" +
		"    args.addAll(['-X', method]);\n" +
		"    if (opts?['headers'] != null) {\n" +
		"        for (var e in (opts!['headers'] as Map).entries) {\n" +
		"            args.addAll(['-H', '${e.key}: ${e.value}']);\n" +
		"        }\n" +
		"    }\n" +
		"    if (opts?['query'] != null) {\n" +
		"        var qs = Uri(queryParameters: (opts!['query'] as Map).map((k,v)=>MapEntry(k.toString(), v.toString()))).query;\n" +
		"        var sep = url.contains('?') ? '&' : '?';\n" +
		"        url = url + sep + qs;\n" +
		"    }\n" +
		"    if (opts != null && opts.containsKey('body')) {\n" +
		"        args.addAll(['-d', jsonEncode(opts['body'])]);\n" +
		"    }\n" +
		"    if (opts?['timeout'] != null) {\n" +
		"        args.addAll(['--max-time', opts!['timeout'].toString()]);\n" +
		"    }\n" +
		"    args.add(url);\n" +
		"    var res = Process.runSync('curl', args);\n" +
		"    return jsonDecode(res.stdout.toString());\n" +
		"}\n"
	helperLoad = "List<Map<String,dynamic>> _load(String? path, Map<String,dynamic>? opts) {\n" +
		"    var format = (opts?['format'] ?? 'csv').toString();\n" +
		"    var header = opts?['header'] ?? true;\n" +
		"    var delim = (opts?['delimiter'] ?? ',').toString();\n" +
		"    if (delim.isEmpty) delim = ',';\n" +
		"    if (format == 'tsv') delim = '\t';\n" +
		"    String text;\n" +
		"    if (path == null || path == '' || path == '-') {\n" +
		"        var lines = <String>[];\n" +
		"        while (true) {\n" +
		"            var line = stdin.readLineSync();\n" +
		"            if (line == null) break;\n" +
		"            lines.add(line);\n" +
		"        }\n" +
		"        text = lines.join('\\n');\n" +
		"    } else {\n" +
		"        text = File(path).readAsStringSync();\n" +
		"    }\n" +
		"    if (format == 'json') {\n" +
		"        var data = jsonDecode(text);\n" +
		"        if (data is List) return data.map((e) => Map<String,dynamic>.from(e as Map)).toList();\n" +
		"        if (data is Map) return [Map<String,dynamic>.from(data)];\n" +
		"        return <Map<String,dynamic>>[];\n" +
		"    }\n" +
		"    if (format == 'jsonl') {\n" +
		"        return text.trim().split(RegExp('\\r?\\n')).where((l) => l.isNotEmpty).map((l) => Map<String,dynamic>.from(jsonDecode(l))).toList();\n" +
		"    }\n" +
		"    if (format == 'yaml') {\n" +
		"        var data = loadYaml(text);\n" +
		"        if (data is YamlList) return data.map((e) => Map<String,dynamic>.from(e)).toList();\n" +
		"        if (data is YamlMap) return [Map<String,dynamic>.from(data)];\n" +
		"        return <Map<String,dynamic>>[];\n" +
		"    }\n" +
		"    if (format != 'csv') return <Map<String,dynamic>>[];\n" +
		"    var lines = text.trim().split(RegExp('\\r?\\n')).where((l) => l.isNotEmpty).toList();\n" +
		"    if (lines.isEmpty) return <Map<String,dynamic>>[];\n" +
		"    List<String> headers;\n" +
		"    if (header) {\n" +
		"        headers = lines[0].split(delim);\n" +
		"    } else {\n" +
		"        headers = List.generate(lines[0].split(delim).length, (i) => 'c$' + i.toString());\n" +
		"    }\n" +
		"    var start = header ? 1 : 0;\n" +
		"    var out = <Map<String,dynamic>>[];\n" +
		"    for (var i = start; i < lines.length; i++) {\n" +
		"        var parts = lines[i].split(delim);\n" +
		"        var row = <String,dynamic>{};\n" +
		"        for (var j = 0; j < headers.length; j++) {\n" +
		"            row[headers[j]] = j < parts.length ? parts[j] : '';\n" +
		"        }\n" +
		"        out.add(row);\n" +
		"    }\n" +
		"    return out;\n" +
		"}\n"
	helperSave = "void _save(List<Map<String,dynamic>> rows, String? path, Map<String,dynamic>? opts) {\n" +
		"    var format = (opts?['format'] ?? 'csv').toString();\n" +
		"    var header = opts?['header'] ?? false;\n" +
		"    var delim = (opts?['delimiter'] ?? ',').toString();\n" +
		"    if (delim.isEmpty) delim = ',';\n" +
		"    if (format == 'tsv') delim = '\t';\n" +
		"    String text;\n" +
		"    if (format == 'json') {\n" +
		"        text = jsonEncode(rows);\n" +
		"    } else if (format == 'jsonl') {\n" +
		"        text = rows.map((r) => jsonEncode(r)).join('\\n') + '\n';\n" +
		"    } else if (format == 'yaml') {\n" +
		"        var enc = YamlEncoder();\n" +
		"        text = enc.convert(rows.length == 1 ? rows[0] : rows);\n" +
		"    } else if (format == 'csv') {\n" +
		"        var headers = rows.isNotEmpty ? (rows[0].keys.toList()..sort()) : <String>[];\n" +
		"        var lines = <String>[];\n" +
		"        if (header) lines.add(headers.join(delim));\n" +
		"        for (var row in rows) {\n" +
		"            lines.add(headers.map((h) => row[h]?.toString() ?? '').join(delim));\n" +
		"        }\n" +
		"        text = lines.join('\\n') + '\n';\n" +
		"    } else {\n" +
		"        return;\n" +
		"    }\n" +
		"    if (path == null || path == '' || path == '-') {\n" +
		"        stdout.write(text);\n" +
		"    } else {\n" +
		"        File(path).writeAsStringSync(text);\n" +
		"    }\n" +
		"}\n"
	helperQuery = "List<dynamic> _query(List<dynamic> src, List<Map<String,dynamic>> joins, Map<String,dynamic> opts) {\n" +
		"    var items = [for (var v in src) [v]];\n" +
		"    for (var j in joins) {\n" +
		"        var joined = <List<dynamic>>[];\n" +
		"        var jitems = (j['items'] as List).cast<dynamic>();\n" +
		"        var on = j['on'];\n" +
		"        var left = j['left'] == true;\n" +
		"        var right = j['right'] == true;\n" +
		"        if (right && left) {\n" +
		"            var matched = List<bool>.filled(jitems.length, false);\n" +
		"            for (var leftRow in items) {\n" +
		"                var m = false;\n" +
		"                for (var ri = 0; ri < jitems.length; ri++) {\n" +
		"                    var rightRow = jitems[ri];\n" +
		"                    var keep = true;\n" +
		"                    if (on != null) keep = Function.apply(on, [...leftRow, rightRow]) as bool;\n" +
		"                    if (!keep) continue;\n" +
		"                    m = true; matched[ri] = true;\n" +
		"                    joined.add([...leftRow, rightRow]);\n" +
		"                }\n" +
		"                if (!m) joined.add([...leftRow, null]);\n" +
		"            }\n" +
		"            for (var ri = 0; ri < jitems.length; ri++) {\n" +
		"                if (!matched[ri]) {\n" +
		"                    var undef = items.isNotEmpty ? List<dynamic>.filled(items[0].length, null) : <dynamic>[];\n" +
		"                    joined.add([...undef, jitems[ri]]);\n" +
		"                }\n" +
		"            }\n" +
		"        } else if (right) {\n" +
		"            for (var rightRow in jitems) {\n" +
		"                var m = false;\n" +
		"                for (var leftRow in items) {\n" +
		"                    var keep = true;\n" +
		"                    if (on != null) keep = Function.apply(on, [...leftRow, rightRow]) as bool;\n" +
		"                    if (!keep) continue;\n" +
		"                    m = true; joined.add([...leftRow, rightRow]);\n" +
		"                }\n" +
		"                if (!m) {\n" +
		"                    var undef = items.isNotEmpty ? List<dynamic>.filled(items[0].length, null) : <dynamic>[];\n" +
		"                    joined.add([...undef, rightRow]);\n" +
		"                }\n" +
		"            }\n" +
		"        } else {\n" +
		"            for (var leftRow in items) {\n" +
		"                var m = false;\n" +
		"                for (var rightRow in jitems) {\n" +
		"                    var keep = true;\n" +
		"                    if (on != null) keep = Function.apply(on, [...leftRow, rightRow]) as bool;\n" +
		"                    if (!keep) continue;\n" +
		"                    m = true; joined.add([...leftRow, rightRow]);\n" +
		"                }\n" +
		"                if (left && !m) joined.add([...leftRow, null]);\n" +
		"            }\n" +
		"        }\n" +
		"        items = joined;\n" +
		"    }\n" +
		"    if (opts['where'] != null) {\n" +
		"        items = [for (var r in items) if (Function.apply(opts['where'], r) as bool) r];\n" +
		"    }\n" +
		"    if (opts['sortKey'] != null) {\n" +
		"        var pairs = [for (var it in items) {'item': it, 'key': Function.apply(opts['sortKey'], it)}];\n" +
		"        pairs.sort((a,b) {\n" +
		"            var ak = a['key']; var bk = b['key'];\n" +
		"            if (ak is num && bk is num) return ak.compareTo(bk);\n" +
		"            if (ak is String && bk is String) return ak.compareTo(bk);\n" +
		"            return ak.toString().compareTo(bk.toString());\n" +
		"        });\n" +
		"        items = [for (var p in pairs) p['item'] as List<dynamic>];\n" +
		"    }\n" +
		"    if (opts['skip'] != null) {\n" +
		"        var n = opts['skip'] as int;\n" +
		"        items = n < items.length ? items.sublist(n) : <List<dynamic>>[];\n" +
		"    }\n" +
		"    if (opts['take'] != null) {\n" +
		"        var n = opts['take'] as int;\n" +
		"        if (n < items.length) items = items.sublist(0, n);\n" +
		"    }\n" +
		"    var res = <dynamic>[];\n" +
		"    for (var r in items) { res.add(Function.apply(opts['select'], r)); }\n" +
		"    return res;\n" +
		"}\n"
	helperGenText = "String _genText(String prompt, String model, Map<String,dynamic>? params) {\n" +
		"    // TODO: integrate with an LLM\n" +
		"    return prompt;\n" +
		"}\n"
	helperGenEmbed = "List<double> _genEmbed(String text, String model, Map<String,dynamic>? params) {\n" +
		"    return text.codeUnits.map((c) => c.toDouble()).toList();\n" +
		"}\n"
	helperGenStruct = "T _genStruct<T>(String prompt, String model, Map<String,dynamic>? params) {\n" +
		"    // TODO: parse model output into a struct\n" +
		"    return null as T;\n" +
		"}\n"
	helperJson = "void _json(dynamic v) {\n" +
		"    print(jsonEncode(v));\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_indexString": helperIndexString,
	"_unionAll":    helperUnionAll,
	"_union":       helperUnion,
	"_except":      helperExcept,
	"_intersect":   helperIntersect,
	"_Stream":      helperStream,
	"_Agent":       helperAgent,
	"_waitAll":     helperWaitAll,
	"_Group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_fetch":       helperFetch,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_query":       helperQuery,
	"_genText":     helperGenText,
	"_genEmbed":    helperGenEmbed,
	"_genStruct":   helperGenStruct,
	"_json":        helperJson,
}

func (c *Compiler) use(name string) {
	if c.helpers == nil {
		c.helpers = map[string]bool{}
	}
	c.helpers[name] = true
}
