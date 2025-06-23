package rscode

import "sort"

const (
	helperIndexString = "fn _index_string(s: &str, i: i64) -> String {\n" +
		"    let mut idx = i;\n" +
		"    let chars: Vec<char> = s.chars().collect();\n" +
		"    if idx < 0 { idx += chars.len() as i64; }\n" +
		"    if idx < 0 || idx >= chars.len() as i64 { panic!(\"index out of range\"); }\n" +
		"    chars[idx as usize].to_string()\n" +
		"}\n"

	helperMapGet = "fn _map_get<K: std::cmp::Eq + std::hash::Hash, V: Clone>(m: &std::collections::HashMap<K, V>, k: &K) -> V {\n" +
		"    m.get(k).unwrap().clone()\n" +
		"}\n"

	helperCount = "fn _count<T>(v: &[T]) -> i32 {\n" +
		"    v.len() as i32\n" +
		"}\n"

	helperAvg = "fn _avg<T: Into<f64> + Copy>(v: &[T]) -> f64 {\n" +
		"    if v.is_empty() { return 0.0 }\n" +
		"    let mut sum = 0.0;\n" +
		"    for &it in v { sum += Into::<f64>::into(it); }\n" +
		"    sum / v.len() as f64\n" +
		"}\n"

	helperInMap = "fn _in_map<K: std::cmp::Eq + std::hash::Hash, V>(m: &std::collections::HashMap<K, V>, k: &K) -> bool {\n" +
		"    m.contains_key(k)\n" +
		"}\n"

	helperConcat = "fn _concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    let mut res = Vec::with_capacity(a.len() + b.len());\n" +
		"    res.extend_from_slice(a);\n" +
		"    res.extend_from_slice(b);\n" +
		"    res\n" +
		"}\n"
	helperUnionAll = "fn _union_all<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    let mut res = Vec::with_capacity(a.len() + b.len());\n" +
		"    res.extend_from_slice(a);\n" +
		"    res.extend_from_slice(b);\n" +
		"    res\n" +
		"}\n"

	helperUnion = "fn _union<T: PartialEq + Clone>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    let mut res = a.to_vec();\n" +
		"    for it in b {\n" +
		"        if !res.contains(it) { res.push(it.clone()); }\n" +
		"    }\n" +
		"    res\n" +
		"}\n"

	helperExcept = "fn _except<T: PartialEq + Clone>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    let mut res = Vec::new();\n" +
		"    for it in a {\n" +
		"        if !b.contains(it) { res.push(it.clone()); }\n" +
		"    }\n" +
		"    res\n" +
		"}\n"

	helperIntersect = "fn _intersect<T: PartialEq + Clone>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    let mut res = Vec::new();\n" +
		"    for it in a {\n" +
		"        if b.contains(it) && !res.contains(it) { res.push(it.clone()); }\n" +
		"    }\n" +
		"    res\n" +
		"}\n"

	helperInput = "fn _input() -> String {\n" +
		"    use std::io::Read;\n" +
		"    let mut s = String::new();\n" +
		"    std::io::stdin().read_line(&mut s).unwrap();\n" +
		"    s.trim().to_string()\n" +
		"}\n"

	helperGenText = "fn _gen_text(_prompt: &str, _model: &str) -> String {\n" +
		"    String::new()\n" +
		"}\n"

	helperGenEmbed = "fn _gen_embed(_text: &str, _model: &str) -> Vec<f64> {\n" +
		"    Vec::new()\n" +
		"}\n"

	helperFetch = "fn _fetch(_url: &str) -> String {\n" +
		"    String::new()\n" +
		"}\n"

	helperLoad = "fn _load<T>(_path: &str) -> Vec<T> {\n" +
		"    Vec::new()\n" +
		"}\n"

	helperSave = "fn _save<T>(_src: &[T], _path: &str) {\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_index_string": helperIndexString,
	"_map_get":      helperMapGet,
	"_count":        helperCount,
	"_avg":          helperAvg,
	"_in_map":       helperInMap,
	"_input":        helperInput,
	"_concat":       helperConcat,
	"_union_all":    helperUnionAll,
	"_union":        helperUnion,
	"_except":       helperExcept,
	"_intersect":    helperIntersect,
	"_gen_text":     helperGenText,
	"_gen_embed":    helperGenEmbed,
	"_fetch":        helperFetch,
	"_load":         helperLoad,
	"_save":         helperSave,
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

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
	}
}
