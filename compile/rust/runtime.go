package rscode

import "sort"

const (
	helperIndexString = "fn _index_string(s: &str, i: i32) -> String {\n" +
		"    let mut idx = i;\n" +
		"    let chars: Vec<char> = s.chars().collect();\n" +
		"    if idx < 0 { idx += chars.len() as i32; }\n" +
		"    if idx < 0 || idx >= chars.len() as i32 { panic!(\"index out of range\"); }\n" +
		"    chars[idx as usize].to_string()\n" +
		"}\n"

	helperMapGet = "fn _map_get<K: std::cmp::Eq + std::hash::Hash, V: Clone>(m: &std::collections::HashMap<K, V>, k: &K) -> V {\n" +
		"    m.get(k).unwrap().clone()\n" +
		"}\n"

	helperCount = "trait _Countable { fn count(&self) -> i32; }\n" +
		"impl<T> _Countable for &[T] { fn count(&self) -> i32 { self.len() as i32 } }\n" +
		"impl<T> _Countable for &Vec<T> { fn count(&self) -> i32 { self.len() as i32 } }\n" +
		"impl<K, T> _Countable for &_Group<K, T> { fn count(&self) -> i32 { self.Items.len() as i32 } }\n" +
		"fn _count<C: _Countable>(v: C) -> i32 { v.count() }\n"

	helperAvg = "fn _avg<T: Into<f64> + Copy>(v: &[T]) -> f64 {\n" +
		"    if v.is_empty() { return 0.0 }\n" +
		"    let mut sum = 0.0;\n" +
		"    for &it in v { sum += it.into(); }\n" +
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

	helperInput = "fn _input() -> String {\n" +
		"    use std::io::Read;\n" +
		"    let mut s = String::new();\n" +
		"    std::io::stdin().read_line(&mut s).unwrap();\n" +
		"    s.trim().to_string()\n" +
		"}\n"

	helperGroup = "#[derive(Clone)]\n" +
		"struct _Group<K, T> {\n" +
		"    key: K,\n" +
		"    Items: Vec<T>,\n" +
		"}\n"

	helperGroupBy = "fn _group_by<K, T, F>(src: &[T], keyfn: F) -> Vec<_Group<K, T>>\n" +
		"where\n" +
		"    K: std::cmp::Eq + std::hash::Hash + Clone,\n" +
		"    T: Clone,\n" +
		"    F: Fn(&T) -> K,\n" +
		"{\n" +
		"    use std::collections::HashMap;\n" +
		"    let mut map: HashMap<K, usize> = HashMap::new();\n" +
		"    let mut groups: Vec<_Group<K, T>> = Vec::new();\n" +
		"    for it in src {\n" +
		"        let key = keyfn(it);\n" +
		"        if let Some(&idx) = map.get(&key) {\n" +
		"            groups[idx].Items.push(it.clone());\n" +
		"        } else {\n" +
		"            map.insert(key.clone(), groups.len());\n" +
		"            groups.push(_Group { key: key.clone(), Items: vec![it.clone()] });\n" +
		"        }\n" +
		"    }\n" +
		"    groups\n" +
		"}\n"
	helperUnion = "fn _union<T: Clone + std::cmp::Eq + std::hash::Hash>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    use std::collections::HashSet;\n" +
		"    let mut set: HashSet<T> = HashSet::new();\n" +
		"    let mut res = Vec::new();\n" +
		"    for it in a { if set.insert(it.clone()) { res.push(it.clone()); } }\n" +
		"    for it in b { if set.insert(it.clone()) { res.push(it.clone()); } }\n" +
		"    res\n" +
		"}\n"
	helperUnionAll = "fn _union_all<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    let mut res = Vec::with_capacity(a.len() + b.len());\n" +
		"    res.extend_from_slice(a);\n" +
		"    res.extend_from_slice(b);\n" +
		"    res\n" +
		"}\n"
	helperExcept = "fn _except<T: Clone + std::cmp::Eq + std::hash::Hash>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    use std::collections::HashSet;\n" +
		"    let set: HashSet<T> = b.iter().cloned().collect();\n" +
		"    let mut res = Vec::new();\n" +
		"    for it in a { if !set.contains(it) { res.push(it.clone()); } }\n" +
		"    res\n" +
		"}\n"
	helperIntersect = "fn _intersect<T: Clone + std::cmp::Eq + std::hash::Hash>(a: &[T], b: &[T]) -> Vec<T> {\n" +
		"    use std::collections::HashSet;\n" +
		"    let set: HashSet<T> = b.iter().cloned().collect();\n" +
		"    let mut res = Vec::new();\n" +
		"    for it in a { if set.contains(it) { res.push(it.clone()); } }\n" +
		"    res\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_index_string": helperIndexString,
	"_map_get":      helperMapGet,
	"_count":        helperCount,
	"_avg":          helperAvg,
	"_in_map":       helperInMap,
	"_union":        helperUnion,
	"_union_all":    helperUnionAll,
	"_except":       helperExcept,
	"_intersect":    helperIntersect,
	"_group":        helperGroup,
	"_group_by":     helperGroupBy,
	"_input":        helperInput,
	"_concat":       helperConcat,
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
