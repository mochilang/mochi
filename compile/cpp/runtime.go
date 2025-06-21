package cppcode

// Runtime helper data for the C++ backend.

// ordered helper names ensures deterministic output
var helperOrder = []string{"indexString", "sliceVec", "sliceStr", "fmtVec", "groupBy", "reduce", "count", "avg", "union", "except", "intersect", "json", "input"}

// helperCode contains the C++ source for each optional runtime helper
var helperCode = map[string][]string{
	"indexString": {
		"string _indexString(const string& s, int i) {",
		"\tint n = s.size();",
		"\tif (i < 0) i += n;",
		"\tif (i < 0 || i >= n) throw std::out_of_range(\"index out of range\");",
		"\treturn string(1, s[i]);",
		"}",
	},
	"sliceVec": {
		"template<typename T> vector<T> _slice(const vector<T>& v, int start, int end) {",
		"\tint n = v.size();",
		"\tif (start < 0) start += n;",
		"\tif (end < 0) end += n;",
		"\tif (start < 0) start = 0;",
		"\tif (end > n) end = n;",
		"\tif (end < start) end = start;",
		"\treturn vector<T>(v.begin() + start, v.begin() + end);",
		"}",
	},
	"sliceStr": {
		"string _sliceString(const string& s, int start, int end) {",
		"\tint n = s.size();",
		"\tif (start < 0) start += n;",
		"\tif (end < 0) end += n;",
		"\tif (start < 0) start = 0;",
		"\tif (end > n) end = n;",
		"\tif (end < start) end = start;",
		"\treturn s.substr(start, end - start);",
		"}",
	},
	"fmtVec": {
		"template<typename T> string _fmtVec(const vector<T>& v) {",
		"\tstringstream ss;",
		"\tss << '[';",
		"\tfor (size_t i = 0; i < v.size(); i++) {",
		"\t\tif (i > 0) ss << ' ';",
		"\t\tss << v[i];",
		"\t}",
		"\tss << ']';",
		"\treturn ss.str();",
		"}",
	},
	"groupBy": {
		"template<typename Src, typename KeyFunc> auto _group_by(const Src& src, KeyFunc keyfn) {",
		"\tusing ElemT = typename std::decay<decltype(*std::begin(src))>::type;",
		"\tusing KeyT = decltype(keyfn(*std::begin(src)));",
		"\tstruct _Group { KeyT Key; vector<ElemT> Items; };",
		"\tunordered_map<KeyT, _Group> groups;",
		"\tvector<KeyT> order;",
		"\tfor (const auto& it : src) {",
		"\t\tKeyT k = keyfn(it);",
		"\t\tif (!groups.count(k)) { groups[k] = _Group{k, {}}; order.push_back(k); }",
		"\t\tgroups[k].Items.push_back(it);",
		"\t}",
		"\tvector<_Group> res;",
		"\tfor (const auto& k : order) res.push_back(groups[k]);",
		"\treturn res;",
		"}",
	},
	"reduce": {
		"template<typename Src, typename Fn, typename Acc> Acc _reduce(const Src& src, Fn fn, Acc acc) {",
		"\tfor (const auto& it : src) {",
		"\t\tacc = fn(acc, it);",
		"\t}",
		"\treturn acc;",
		"}",
	},
	"count": {
		"template<typename T> auto _count(const T& v) -> decltype(v.size(), int{}) {",
		"\treturn (int)v.size();",
		"}",
		"template<typename T> auto _count(const T& v) -> decltype(v.Items, int{}) {",
		"\treturn (int)v.Items.size();",
		"}",
	},
	"avg": {
		"template<typename T> auto _avg(const T& v) -> decltype(v.size(), double{}) {",
		"\tif (v.size() == 0) return 0;",
		"\tdouble sum = 0;",
		"\tfor (const auto& it : v) sum += it;",
		"\treturn sum / v.size();",
		"}",
		"template<typename T> auto _avg(const T& v) -> decltype(v.Items, double{}) {",
		"\treturn _avg(v.Items);",
		"}",
	},
	"union": {
		"template<typename T> vector<T> _union(const vector<T>& a, const vector<T>& b) {",
		"\tvector<T> res = a;",
		"\tfor (const auto& it : b) {",
		"\t\tif (find(res.begin(), res.end(), it) == res.end()) res.push_back(it);",
		"\t}",
		"\treturn res;",
		"}",
	},
	"except": {
		"template<typename T> vector<T> _except(const vector<T>& a, const vector<T>& b) {",
		"\tvector<T> res;",
		"\tfor (const auto& it : a) {",
		"\t\tif (find(b.begin(), b.end(), it) == b.end()) res.push_back(it);",
		"\t}",
		"\treturn res;",
		"}",
	},
	"intersect": {
		"template<typename T> vector<T> _intersect(const vector<T>& a, const vector<T>& b) {",
		"\tvector<T> res;",
		"\tfor (const auto& it : a) {",
		"\t\tif (find(b.begin(), b.end(), it) != b.end() && find(res.begin(), res.end(), it) == res.end()) res.push_back(it);",
		"\t}",
		"\treturn res;",
		"}",
	},
	"json": {
		"static string _escape_json(const string& s) {",
		"\tstring out;",
		"\tfor (char c : s) {",
		"\t\tif (c == '\"' || c == '\\') out += '\\';",
		"\t\tout += c;",
		"\t}",
		"\treturn out;",
		"}",
		"template<typename T> string _to_json(const T& v);",
		"inline string _to_json(const string& s) {",
		"\tstring out = \"\\\"\";",
		"\tout += _escape_json(s);",
		"\tout += \"\\\"\";",
		"\treturn out;",
		"}",
		"inline string _to_json(const char* s) { return _to_json(string(s)); }",
		"inline string _to_json(int v) { return to_string(v); }",
		"inline string _to_json(double v) { stringstream ss; ss << v; return ss.str(); }",
		"inline string _to_json(bool v) { return v ? \"true\" : \"false\"; }",
		"template<typename T> string _to_json(const vector<T>& v) {",
		"\tstring out = \"[\";",
		"\tfor (size_t i=0;i<v.size();i++) { if (i>0) out += ','; out += _to_json(v[i]); }",
		"\tout += ']';",
		"\treturn out;",
		"}",
		"template<typename K, typename V> string _to_json(const unordered_map<K,V>& m) {",
		"\tstring out = \"{\"; bool first = true;",
		"\tfor (const auto& kv : m) {",
		"\t\tif (!first) out += ','; first = false;",
		"\t\tout += _to_json(kv.first); out += ':'; out += _to_json(kv.second);",
		"\t}",
		"\tout += '}';",
		"\treturn out;",
		"}",
		"template<typename T> string _to_json(const T& v) { stringstream ss; ss << v; return _to_json(ss.str()); }",
		"template<typename T> void _json(const T& v) { cout << _to_json(v) << endl; }",
	},
	"input": {
		"string _input() {",
		"\tstring s;",
		"\tgetline(cin, s);",
		"\treturn s;",
		"}",
	},
}

// emitRuntime writes used helper functions into the compiler buffer.
func (c *Compiler) emitRuntime() {
	for _, name := range helperOrder {
		if !c.helpers[name] {
			continue
		}
		for _, line := range helperCode[name] {
			c.writeln(line)
		}
		c.writeln("")
	}
}
