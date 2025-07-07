//go:build archived

package cscode

func (c *Compiler) emitRuntime() {
	if len(c.helpers) > 0 {
		for name := range c.helpers {
			switch name {
			case "_count":
				c.writeln("static int _count(dynamic v) {")
				c.indent++
				c.writeln("if (v is string) {")
				c.indent++
				c.writeln("return ((string)v).Length;")
				c.indent--
				c.writeln("}")
				c.writeln("if (v is System.Collections.ICollection c) {")
				c.indent++
				c.writeln("return c.Count;")
				c.indent--
				c.writeln("}")
				c.writeln("throw new Exception(\"count() expects list or string\");")
				c.indent--
				c.writeln("}")
			case "_avg":
				c.writeln("static double _avg(dynamic v) {")
				c.indent++
				c.writeln("if (v == null) return 0.0;")
				c.writeln("int _n = 0;")
				c.writeln("double _sum = 0;")
				c.writeln("foreach (var it in v) {")
				c.indent++
				c.writeln("_sum += Convert.ToDouble(it);")
				c.writeln("_n++;")
				c.indent--
				c.writeln("}")
				c.writeln("return _n == 0 ? 0.0 : _sum / _n;")
				c.indent--
				c.writeln("}")
			case "_sum":
				c.writeln("static double _sum(dynamic v) {")
				c.indent++
				c.writeln("if (v == null) return 0.0;")
				c.writeln("double _sum = 0;")
				c.writeln("foreach (var it in v) {")
				c.indent++
				c.writeln("_sum += Convert.ToDouble(it);")
				c.indent--
				c.writeln("}")
				c.writeln("return _sum;")
				c.indent--
				c.writeln("}")
			case "_exists":
				c.writeln("static bool _exists(dynamic v) {")
				c.indent++
				c.writeln("if (v is _Group g) v = g.Items;")
				c.writeln("if (v is string s) return s.Length > 0;")
				c.writeln("if (v is System.Collections.IDictionary d) return d.Count > 0;")
				c.writeln("if (v is System.Collections.IEnumerable e) {")
				c.indent++
				c.writeln("foreach (var _ in e) return true;")
				c.writeln("return false;")
				c.indent--
				c.writeln("}")
				c.writeln("return v != null;")
				c.indent--
				c.writeln("}")
			case "_min":
				c.writeln("static dynamic _min(dynamic v) {")
				c.indent++
				c.writeln("if (v == null) return 0;")
				c.writeln("System.Collections.IEnumerable list = v is _Group g ? g.Items : v as System.Collections.IEnumerable;")
				c.writeln("if (list == null) return 0;")
				c.writeln("var it = list.GetEnumerator();")
				c.writeln("if (!it.MoveNext()) return 0;")
				c.writeln("dynamic m = it.Current;")
				c.writeln("var cmp = System.Collections.Generic.Comparer<dynamic>.Default;")
				c.writeln("while (it.MoveNext()) {")
				c.indent++
				c.writeln("dynamic x = it.Current; if (cmp.Compare(x, m) < 0) m = x;")
				c.indent--
				c.writeln("}")
				c.writeln("return m;")
				c.indent--
				c.writeln("}")
			case "_max":
				c.writeln("static dynamic _max(dynamic v) {")
				c.indent++
				c.writeln("if (v == null) return 0;")
				c.writeln("System.Collections.IEnumerable list = v is _Group g ? g.Items : v as System.Collections.IEnumerable;")
				c.writeln("if (list == null) return 0;")
				c.writeln("var it = list.GetEnumerator();")
				c.writeln("if (!it.MoveNext()) return 0;")
				c.writeln("dynamic m = it.Current;")
				c.writeln("var cmp = System.Collections.Generic.Comparer<dynamic>.Default;")
				c.writeln("while (it.MoveNext()) {")
				c.indent++
				c.writeln("dynamic x = it.Current; if (cmp.Compare(x, m) > 0) m = x;")
				c.indent--
				c.writeln("}")
				c.writeln("return m;")
				c.indent--
				c.writeln("}")
			case "_in":
				c.writeln("static bool _in(dynamic item, dynamic col) {")
				c.indent++
				c.writeln("if (col is string s && item is string sub) {")
				c.indent++
				c.writeln("return s.Contains(sub);")
				c.indent--
				c.writeln("}")
				c.writeln("if (col is System.Collections.IDictionary d) {")
				c.indent++
				c.writeln("return d.Contains(item);")
				c.indent--
				c.writeln("}")
				c.writeln("if (col is System.Collections.IEnumerable e) {")
				c.indent++
				c.writeln("foreach (var it in e) {")
				c.indent++
				c.writeln("if (Equals(it, item)) return true;")
				c.indent--
				c.writeln("}")
				c.writeln("return false;")
				c.indent--
				c.writeln("}")
				c.writeln("return false;")
				c.indent--
				c.writeln("}")
			case "_cast":
				c.writeln("static T _cast<T>(dynamic v) {")
				c.indent++
				c.writeln("if (v is T tv) return tv;")
				c.writeln("if (typeof(T) == typeof(int)) {")
				c.indent++
				c.writeln("if (v is int) return (T)v;")
				c.writeln("if (v is double) return (T)(object)(int)(double)v;")
				c.writeln("if (v is float) return (T)(object)(int)(float)v;")
				c.indent--
				c.writeln("}")
				c.writeln("if (typeof(T) == typeof(double)) {")
				c.indent++
				c.writeln("if (v is int) return (T)(object)(double)(int)v;")
				c.writeln("if (v is double) return (T)v;")
				c.writeln("if (v is float) return (T)(object)(double)(float)v;")
				c.indent--
				c.writeln("}")
				c.writeln("if (typeof(T) == typeof(float)) {")
				c.indent++
				c.writeln("if (v is int) return (T)(object)(float)(int)v;")
				c.writeln("if (v is double) return (T)(object)(float)(double)v;")
				c.writeln("if (v is float) return (T)v;")
				c.indent--
				c.writeln("}")
				c.writeln("if (typeof(T).IsGenericType && typeof(T).GetGenericTypeDefinition() == typeof(Dictionary<,>) && v is System.Collections.IDictionary d) {")
				c.indent++
				c.writeln("var args = typeof(T).GetGenericArguments();")
				c.writeln("var res = (System.Collections.IDictionary)Activator.CreateInstance(typeof(Dictionary<,>).MakeGenericType(args));")
				c.writeln("var mCast = typeof(Program).GetMethod(\"_cast\");")
				c.writeln("foreach (System.Collections.DictionaryEntry kv in d) {")
				c.indent++
				c.writeln("var k = mCast.MakeGenericMethod(args[0]).Invoke(null, new object[]{kv.Key});")
				c.writeln("var val = mCast.MakeGenericMethod(args[1]).Invoke(null, new object[]{kv.Value});")
				c.writeln("res.Add(k, val);")
				c.indent--
				c.writeln("}")
				c.writeln("return (T)res;")
				c.indent--
				c.writeln("}")
				c.writeln("if (v is System.Collections.Generic.IDictionary<object, object> dm) {")
				c.indent++
				c.writeln("var m = new Dictionary<string, object>();")
				c.writeln("foreach (var kv in dm) m[Convert.ToString(kv.Key)] = kv.Value;")
				c.writeln("v = m;")
				c.indent--
				c.writeln("}")
				c.writeln("var json = JsonSerializer.Serialize(v);")
				c.writeln("return JsonSerializer.Deserialize<T>(json);")
				c.indent--
				c.writeln("}")
			case "_indexString":
				c.writeln("static string _indexString(string s, long i) {")
				c.indent++
				c.writeln("if (i < 0) i += s.Length;")
				c.writeln("if (i < 0 || i >= s.Length) throw new Exception(\"index out of range\");")
				c.writeln("return s[(int)i].ToString();")
				c.indent--
				c.writeln("}")
			case "_sliceString":
				c.writeln("static string _sliceString(string s, long i, long j) {")
				c.indent++
				c.writeln("var start = i;")
				c.writeln("var end = j;")
				c.writeln("var n = s.Length;")
				c.writeln("if (start < 0) start += n;")
				c.writeln("if (end < 0) end += n;")
				c.writeln("if (start < 0) start = 0;")
				c.writeln("if (end > n) end = n;")
				c.writeln("if (end < start) end = start;")
				c.writeln("return s.Substring((int)start, (int)(end - start));")
				c.indent--
				c.writeln("}")
			case "_indexList":
				c.writeln("static dynamic _indexList(dynamic l, long i) {")
				c.indent++
				c.writeln("var list = l as System.Collections.IList;")
				c.writeln("if (list == null) throw new Exception(\"index() expects list\");")
				c.writeln("if (i < 0) i += list.Count;")
				c.writeln("if (i < 0 || i >= list.Count) throw new Exception(\"index out of range\");")
				c.writeln("return list[(int)i];")
				c.indent--
				c.writeln("}")
			case "_sliceList":
				c.writeln("static List<dynamic> _sliceList(dynamic l, long i, long j) {")
				c.indent++
				c.writeln("var list = l as System.Collections.IList;")
				c.writeln("if (list == null) return new List<dynamic>();")
				c.writeln("var start = i;")
				c.writeln("var end = j;")
				c.writeln("var n = list.Count;")
				c.writeln("if (start < 0) start += n;")
				c.writeln("if (end < 0) end += n;")
				c.writeln("if (start < 0) start = 0;")
				c.writeln("if (end > n) end = n;")
				c.writeln("if (end < start) end = start;")
				c.writeln("var res = new List<dynamic>();")
				c.writeln("for (int k = (int)start; k < (int)end; k++) res.Add(list[k]);")
				c.writeln("return res;")
				c.indent--
				c.writeln("}")
			case "_equal":
				c.writeln("static bool _equal(dynamic a, dynamic b) {")
				c.indent++
				c.writeln("if (a is System.Collections.IEnumerable ae && b is System.Collections.IEnumerable be && a is not string && b is not string) {")
				c.indent++
				c.writeln("var ea = ae.GetEnumerator();")
				c.writeln("var eb = be.GetEnumerator();")
				c.writeln("while (true) {")
				c.indent++
				c.writeln("bool ha = ea.MoveNext();")
				c.writeln("bool hb = eb.MoveNext();")
				c.writeln("if (ha != hb) return false;")
				c.writeln("if (!ha) break;")
				c.writeln("if (!_equal(ea.Current, eb.Current)) return false;")
				c.indent--
				c.writeln("}")
				c.writeln("return true;")
				c.indent--
				c.writeln("}")
				c.writeln("return Equals(a, b);")
				c.indent--
				c.writeln("}")
			case "_union_all":
				c.writeln("static List<dynamic> _union_all(IEnumerable<dynamic> a, IEnumerable<dynamic> b) {")
				c.indent++
				c.writeln("var res = new List<dynamic>();")
				c.writeln("if (a != null) foreach (var it in a) res.Add(it);")
				c.writeln("if (b != null) foreach (var it in b) res.Add(it);")
				c.writeln("return res;")
				c.indent--
				c.writeln("}")
			case "_union":
				c.writeln("static List<dynamic> _union(IEnumerable<dynamic> a, IEnumerable<dynamic> b) {")
				c.indent++
				c.writeln("var res = new List<dynamic>();")
				c.writeln("if (a != null) foreach (var it in a) if (!_in(it, res)) res.Add(it);")
				c.writeln("if (b != null) foreach (var it in b) if (!_in(it, res)) res.Add(it);")
				c.writeln("return res;")
				c.indent--
				c.writeln("}")
			case "_except":
				c.writeln("static List<dynamic> _except(IEnumerable<dynamic> a, IEnumerable<dynamic> b) {")
				c.indent++
				c.writeln("var res = new List<dynamic>();")
				c.writeln("if (a != null) foreach (var it in a) if (!_in(it, b)) res.Add(it);")
				c.writeln("return res;")
				c.indent--
				c.writeln("}")
			case "_intersect":
				c.writeln("static List<dynamic> _intersect(IEnumerable<dynamic> a, IEnumerable<dynamic> b) {")
				c.indent++
				c.writeln("var res = new List<dynamic>();")
				c.writeln("if (a != null) foreach (var it in a) if (_in(it, b) && !_in(it, res)) res.Add(it);")
				c.writeln("return res;")
				c.indent--
				c.writeln("}")
			case "_group":
				c.writeln("public class _Group {")
				c.indent++
				c.writeln("public dynamic key;")
				c.writeln("public List<dynamic> Items = new List<dynamic>();")
				c.writeln("public _Group(dynamic k) { key = k; }")
				c.indent--
				c.writeln("}")
			case "_group_by":
				c.writeln("static List<_Group> _group_by(IEnumerable<dynamic> src, Func<dynamic, dynamic> keyfn) {")
				c.indent++
				c.writeln("var groups = new Dictionary<string, _Group>();")
				c.writeln("var order = new List<string>();")
				c.writeln("foreach (var it in src) {")
				c.indent++
				c.writeln("var key = keyfn(it);")
				c.writeln("var ks = Convert.ToString(key);")
				c.writeln("if (!groups.TryGetValue(ks, out var g)) {")
				c.indent++
				c.writeln("g = new _Group(key);")
				c.writeln("groups[ks] = g;")
				c.writeln("order.Add(ks);")
				c.indent--
				c.writeln("}")
				c.writeln("g.Items.Add(it);")
				c.indent--
				c.writeln("}")
				c.writeln("var res = new List<_Group>();")
				c.writeln("foreach (var k in order) res.Add(groups[k]);")
				c.writeln("return res;")
				c.indent--
				c.writeln("}")
			case "_fetch":
				c.writeln("static dynamic _fetch(string url, Dictionary<string, object> opts) {")
				c.indent++
				c.writeln("var method = \"GET\";")
				c.writeln("if (opts != null && opts.ContainsKey(\"method\")) method = Convert.ToString(opts[\"method\"]);")
				c.writeln("var query = opts != null && opts.ContainsKey(\"query\") ? opts[\"query\"] as Dictionary<string, object> : null;")
				c.writeln("if (query != null) {")
				c.indent++
				c.writeln("var qs = System.Web.HttpUtility.ParseQueryString(\"\");")
				c.writeln("foreach (var kv in query) qs[kv.Key] = Convert.ToString(kv.Value);")
				c.writeln("var sep = url.Contains('?') ? '&' : '?';")
				c.writeln("url = url + sep + qs.ToString();")
				c.indent--
				c.writeln("}")
				c.writeln("var req = new HttpRequestMessage(new HttpMethod(method), url);")
				c.writeln("if (opts != null && opts.ContainsKey(\"headers\")) {")
				c.indent++
				c.writeln("var hs = opts[\"headers\"] as Dictionary<string, object>;")
				c.writeln("if (hs != null) foreach (var kv in hs) req.Headers.TryAddWithoutValidation(kv.Key, Convert.ToString(kv.Value));")
				c.indent--
				c.writeln("}")
				c.writeln("if (opts != null && opts.ContainsKey(\"body\")) {")
				c.indent++
				c.writeln("var data = JsonSerializer.Serialize(opts[\"body\"]);")
				c.writeln("req.Content = new StringContent(data, System.Text.Encoding.UTF8, \"application/json\");")
				c.indent--
				c.writeln("}")
				c.writeln("var client = new HttpClient();")
				c.writeln("if (opts != null && opts.ContainsKey(\"timeout\")) {")
				c.indent++
				c.writeln("var t = Convert.ToDouble(opts[\"timeout\"]);")
				c.writeln("client.Timeout = TimeSpan.FromSeconds(t);")
				c.indent--
				c.writeln("}")
				c.writeln("var resp = client.Send(req);")
				c.writeln("var text = resp.Content.ReadAsStringAsync().Result;")
				c.writeln("return JsonSerializer.Deserialize<dynamic>(text);")
				c.indent--
				c.writeln("}")
			case "_load":
				c.writeln("static List<dynamic> _load(string path, Dictionary<string, object> opts) {")
				c.indent++
				c.writeln("var format = opts != null && opts.ContainsKey(\"format\") ? Convert.ToString(opts[\"format\"]) : \"csv\";")
				c.writeln("var header = opts != null && opts.ContainsKey(\"header\") ? Convert.ToBoolean(opts[\"header\"]) : true;")
				c.writeln("var delim = opts != null && opts.ContainsKey(\"delimiter\") ? Convert.ToString(opts[\"delimiter\"])[0] : ',';")
				c.writeln("string text;")
				c.writeln("if (string.IsNullOrEmpty(path) || path == \"-\") {")
				c.indent++
				c.writeln("text = Console.In.ReadToEnd();")
				c.indent--
				c.writeln("} else {")
				c.indent++
				c.writeln("text = File.ReadAllText(path);")
				c.indent--
				c.writeln("}")
				c.writeln("switch (format) {")
				c.writeln("case \"jsonl\":")
				c.indent++
				c.writeln("var list = new List<dynamic>();")
				c.writeln("foreach (var line in text.Split(new[] { '\\n', '\\r' }, StringSplitOptions.RemoveEmptyEntries)) list.Add(JsonSerializer.Deserialize<dynamic>(line));")
				c.writeln("return list;")
				c.indent--
				c.writeln("case \"json\":")
				c.indent++
				c.writeln("return JsonSerializer.Deserialize<List<dynamic>>(text);")
				c.indent--
				c.writeln("case \"yaml\":")
				c.indent++
				c.writeln("var deser = new DeserializerBuilder().Build();")
				c.writeln("var obj = deser.Deserialize<object>(new StringReader(text));")
				c.writeln("if (obj is IList<object> lst) return lst.Cast<dynamic>().ToList();")
				c.writeln("if (obj is IDictionary<object, object> m) {")
				c.indent++
				c.writeln("var d = new Dictionary<string, object>();")
				c.writeln("foreach (var kv in m) d[Convert.ToString(kv.Key)] = kv.Value;")
				c.writeln("return new List<dynamic> { d };")
				c.indent--
				c.writeln("}")
				c.writeln("return new List<dynamic>();")
				c.indent--
				c.writeln("case \"tsv\":")
				c.indent++
				c.writeln("delim = '\t'; goto default;")
				c.indent--
				c.writeln("default:")
				c.indent++
				c.writeln("var lines = text.Split(new[] { '\\n', '\\r' }, StringSplitOptions.RemoveEmptyEntries);")
				c.writeln("var out = new List<dynamic>();")
				c.writeln("string[] headers = null;")
				c.writeln("for (int i = 0; i < lines.Length; i++) {")
				c.indent++
				c.writeln("var parts = lines[i].Split(delim);")
				c.writeln("if (i == 0 && header) { headers = parts; continue; }")
				c.writeln("var obj = new Dictionary<string, object>();")
				c.writeln("for (int j = 0; j < parts.Length; j++) {")
				c.indent++
				c.writeln("var key = headers != null && j < headers.Length ? headers[j] : $\"col{j}\";")
				c.writeln("obj[key] = parts[j];")
				c.indent--
				c.writeln("}")
				c.writeln("out.Add(obj);")
				c.indent--
				c.writeln("}")
				c.writeln("return out;")
				c.indent--
				c.writeln("}")
				c.indent--
				c.writeln("}")
			case "_save":
				c.writeln("static void _save(dynamic src, string path, Dictionary<string, object> opts) {")
				c.indent++
				c.writeln("var rows = src as IEnumerable<dynamic>; if (rows == null) return;")
				c.writeln("var format = opts != null && opts.ContainsKey(\"format\") ? Convert.ToString(opts[\"format\"]) : \"csv\";")
				c.writeln("var header = opts != null && opts.ContainsKey(\"header\") ? Convert.ToBoolean(opts[\"header\"]) : false;")
				c.writeln("var delim = opts != null && opts.ContainsKey(\"delimiter\") ? Convert.ToString(opts[\"delimiter\"])[0] : ',';")
				c.writeln("switch (format) {")
				c.writeln("case \"jsonl\":")
				c.indent++
				c.writeln("var lines = rows.Select(r => JsonSerializer.Serialize(r));")
				c.writeln("if (string.IsNullOrEmpty(path) || path == \"-\") Console.WriteLine(string.Join(\"\\n\", lines)); else File.WriteAllLines(path, lines);")
				c.writeln("break;")
				c.indent--
				c.writeln("case \"json\":")
				c.indent++
				c.writeln("var data = JsonSerializer.Serialize(rows);")
				c.writeln("if (string.IsNullOrEmpty(path) || path == \"-\") Console.Write(data); else File.WriteAllText(path, data);")
				c.writeln("break;")
				c.indent--
				c.writeln("case \"yaml\":")
				c.indent++
				c.writeln("var ser = new SerializerBuilder().Build();")
				c.writeln("var list = rows.ToList();")
				c.writeln("var data = list.Count == 1 ? list[0] : (object)list;")
				c.writeln("var ydata = ser.Serialize(data);")
				c.writeln("if (string.IsNullOrEmpty(path) || path == \"-\") Console.Write(ydata); else File.WriteAllText(path, ydata);")
				c.writeln("break;")
				c.indent--
				c.writeln("case \"tsv\":")
				c.indent++
				c.writeln("delim = '\t'; goto default;")
				c.indent--
				c.writeln("default:")
				c.indent++
				c.writeln("var list = rows.Cast<IDictionary<string, object>>().ToList();")
				c.writeln("var headers = list.Count > 0 ? list[0].Keys.ToList() : new List<string>();")
				c.writeln("var lines = new List<string>();")
				c.writeln("if (header) lines.Add(string.Join(delim.ToString(), headers));")
				c.writeln("foreach (var row in list) lines.Add(string.Join(delim.ToString(), headers.Select(h => row.ContainsKey(h) ? Convert.ToString(row[h]) : \"\")));")
				c.writeln("if (string.IsNullOrEmpty(path) || path == \"-\") Console.WriteLine(string.Join(\"\\n\", lines)); else File.WriteAllLines(path, lines);")
				c.writeln("break;")
				c.indent--
				c.writeln("}")
				c.indent--
				c.writeln("}")
			case "_eval":
				c.writeln("static dynamic _eval(string code) {")
				c.indent++
				c.writeln("var dt = new System.Data.DataTable();")
				c.writeln("return dt.Compute(code, string.Empty);")
				c.indent--
				c.writeln("}")
			case "_genText":
				c.writeln("static string _genText(string prompt, string model, Dictionary<string, object> p) {")
				c.indent++
				c.writeln("return prompt;")
				c.indent--
				c.writeln("}")
			case "_genEmbed":
				c.writeln("static double[] _genEmbed(string text, string model, Dictionary<string, object> p) {")
				c.indent++
				c.writeln("var vec = new double[text.Length];")
				c.writeln("for (int i = 0; i < text.Length; i++) {")
				c.indent++
				c.writeln("vec[i] = text[i];")
				c.indent--
				c.writeln("}")
				c.writeln("return vec;")
				c.indent--
				c.writeln("}")
			case "_genStruct":
				c.writeln("static T _genStruct<T>(string prompt, string model, Dictionary<string, object> p) {")
				c.indent++
				c.writeln("return JsonSerializer.Deserialize<T>(prompt);")
				c.indent--
				c.writeln("}")
			case "_expect":
				c.writeln("static void expect(bool cond) {")
				c.indent++
				c.writeln("if (!cond) throw new Exception(\"expect failed\");")
				c.indent--
				c.writeln("}")
			case "_agent":
				c.writeln("class _Agent {")
				c.indent++
				c.writeln("public string name;")
				c.writeln("public Dictionary<string, Func<dynamic[], dynamic>> intents = new Dictionary<string, Func<dynamic[], dynamic>>();")
				c.writeln("public Dictionary<string, Action<dynamic>> handlers = new Dictionary<string, Action<dynamic>>();")
				c.writeln("public Dictionary<string, dynamic> state = new Dictionary<string, dynamic>();")
				c.writeln("public _Agent(string n) { name = n; }")
				c.writeln("public void On(_Stream<dynamic> s, Action<dynamic> h) { s.Register(h); }")
				c.writeln("public void RegisterIntent(string n, Func<dynamic[], dynamic> h) { intents[n] = h; }")
				c.writeln("public dynamic Call(string n, params dynamic[] args) { if (!intents.ContainsKey(n)) throw new Exception(\"unknown intent: \" + n); return intents[n](args); }")
				c.writeln("public void Start() { }")
				c.writeln("public void Set(string k, dynamic v) { state[k] = v; }")
				c.writeln("public dynamic Get(string k) { return state.ContainsKey(k) ? state[k] : null; }")
				c.indent--
				c.writeln("}")
			}
			c.writeln("")
		}
	}
}
