//go:build archived

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

	helperSliceString = "fn _slice_string(s: &str, start: i64, end: i64) -> String {\n" +
		"    let mut sidx = start;\n" +
		"    let mut eidx = end;\n" +
		"    let chars: Vec<char> = s.chars().collect();\n" +
		"    let n = chars.len() as i64;\n" +
		"    if sidx < 0 { sidx += n; }\n" +
		"    if eidx < 0 { eidx += n; }\n" +
		"    if sidx < 0 { sidx = 0; }\n" +
		"    if eidx > n { eidx = n; }\n" +
		"    if eidx < sidx { eidx = sidx; }\n" +
		"    chars[sidx as usize..eidx as usize].iter().collect()\n" +
		"}\n"

	helperMapGet = "fn _map_get(m: &std::collections::HashMap<String, std::rc::Rc<dyn std::any::Any>>, k: &String) -> std::rc::Rc<dyn std::any::Any> {\n" +
		"    m.get(k).unwrap().clone()\n" +
		"}\n"

	helperCastInt = "fn _cast_int(v: std::rc::Rc<dyn std::any::Any>) -> i64 {\n" +
		"    if let Some(i) = v.downcast_ref::<i64>() { return *i; }\n" +
		"    if let Some(i) = v.downcast_ref::<i32>() { return *i as i64; }\n" +
		"    if let Some(f) = v.downcast_ref::<f64>() { return *f as i64; }\n" +
		"    panic!(\"cast_int failed\");\n" +
		"}\n"

	helperCastFloat = "fn _cast_float(v: std::rc::Rc<dyn std::any::Any>) -> f64 {\n" +
		"    if let Some(f) = v.downcast_ref::<f64>() { return *f; }\n" +
		"    if let Some(i) = v.downcast_ref::<i64>() { return *i as f64; }\n" +
		"    if let Some(i) = v.downcast_ref::<i32>() { return *i as f64; }\n" +
		"    panic!(\"cast_float failed\");\n" +
		"}\n"

	helperCastString = "fn _cast_string(v: std::rc::Rc<dyn std::any::Any>) -> String {\n" +
		"    if let Some(s) = v.downcast_ref::<String>() { return s.clone(); }\n" +
		"    panic!(\"cast_string failed\");\n" +
		"}\n"

	helperCastBool = "fn _cast_bool(v: std::rc::Rc<dyn std::any::Any>) -> bool {\n" +
		"    if let Some(b) = v.downcast_ref::<bool>() { return *b; }\n" +
		"    panic!(\"cast_bool failed\");\n" +
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

	helperSum = "fn _sum<T: Into<f64> + Copy>(v: &[T]) -> f64 {\n" +
		"    if v.is_empty() { return 0.0 }\n" +
		"    let mut sum = 0.0;\n" +
		"    for &it in v { sum += Into::<f64>::into(it); }\n" +
		"    sum\n" +
		"}\n"

	helperInMap = "fn _in_map<K: std::cmp::Eq + std::hash::Hash, V>(m: &std::collections::HashMap<K, V>, k: &K) -> bool {\n" +
		"    m.contains_key(k)\n" +
		"}\n"

	helperInString = "fn _in_string(s: &str, sub: &str) -> bool {\n" +
		"    s.contains(sub)\n" +
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

	helperFetch = "fn _fetch<T: serde::de::DeserializeOwned>(_url: &str, _opts: std::collections::HashMap<String, String>) -> T {\n" +
		"    use std::process::Command;\n" +
		"    let mut data = String::new();\n" +
		"    if _url.starts_with(\"file://\") {\n" +
		"        if let Ok(text) = std::fs::read_to_string(&_url[7..]) {\n" +
		"            data = text;\n" +
		"        }\n" +
		"    } else {\n" +
		"        let out = Command::new(\"curl\").arg(\"-s\").arg(_url).output().unwrap();\n" +
		"        data = String::from_utf8_lossy(&out.stdout).to_string();\n" +
		"    }\n" +
		"    serde_json::from_str::<T>(&data).unwrap()\n" +
		"}\n"

	helperLoad = "fn _load<T: serde::de::DeserializeOwned>(_path: &str, _opts: std::collections::HashMap<String, String>) -> Vec<T> {\n" +
		"    use std::io::Read;\n" +
		"    let mut data = String::new();\n" +
		"    if _path.is_empty() || _path == \"-\" {\n" +
		"        std::io::stdin().read_to_string(&mut data).unwrap();\n" +
		"    } else if let Ok(mut f) = std::fs::File::open(_path) {\n" +
		"        f.read_to_string(&mut data).unwrap();\n" +
		"    }\n" +
		"    if let Ok(v) = serde_json::from_str::<Vec<T>>(&data) { return v; }\n" +
		"    if let Ok(v) = serde_json::from_str::<T>(&data) { return vec![v]; }\n" +
		"    Vec::new()\n" +
		"}\n"

	helperSave = "fn _save<T: serde::Serialize>(_src: &[T], _path: &str, _opts: std::collections::HashMap<String, String>) {\n" +
		"    if let Ok(text) = serde_json::to_string(_src) {\n" +
		"        if _path.is_empty() || _path == \"-\" {\n" +
		"            println!(\"{}\", text);\n" +
		"        } else {\n" +
		"            std::fs::write(_path, text).unwrap();\n" +
		"        }\n" +
		"    }\n" +
		"}\n"

	helperExpect = "fn expect(cond: bool) {\n" +
		"    if !cond { panic!(\"expect failed\"); }\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_index_string": helperIndexString,
	"_slice_string": helperSliceString,
	"_map_get":      helperMapGet,
	"_cast_int":     helperCastInt,
	"_cast_float":   helperCastFloat,
	"_cast_string":  helperCastString,
	"_cast_bool":    helperCastBool,
	"_count":        helperCount,
	"_avg":          helperAvg,
	"_sum":          helperSum,
	"_in_map":       helperInMap,
	"_in_string":    helperInString,
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
	"expect":        helperExpect,
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
