<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function sortPairs($xs) {
  $arr = $xs;
  $i = 1;
  while ($i < count($arr)) {
  $j = $i;
  while ($j > 0 && (intval($arr[$j - 1]['count'])) < (intval($arr[$j]['count']))) {
  $tmp = $arr[$j - 1];
  $arr[$j - 1] = $arr[$j];
  $arr[$j] = $tmp;
  $j = $j - 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function isAlphaNumDot($ch) {
  return ($ch >= 'A' && $ch <= 'Z') || ($ch >= 'a' && $ch <= 'z') || ($ch >= '0' && $ch <= '9') || $ch == '_' || $ch == '.';
};
  function main() {
  $srcLines = ['package main', '', 'import (', '    "fmt"', '    "go/ast"', '    "go/parser"', '    "go/token"', '    "io/ioutil"', '    "os"', '    "sort"', ')', '', 'func main() {', '    if len(os.Args) != 2 {', '        fmt.Println("usage ff <go source filename>")', '        return', '    }', '    src, err := ioutil.ReadFile(os.Args[1])', '    if err != nil {', '        fmt.Println(err)', '        return', '    }', '    fs := token.NewFileSet()', '    a, err := parser.ParseFile(fs, os.Args[1], src, 0)', '    if err != nil {', '        fmt.Println(err)', '        return', '    }', '    f := fs.File(a.Pos())', '    m := make(map[string]int)', '    ast.Inspect(a, func(n ast.Node) bool {', '        if ce, ok := n.(*ast.CallExpr); ok {', '            start := f.Offset(ce.Pos())', '            end := f.Offset(ce.Lparen)', '            m[string(src[start:end])]++', '        }', '        return true', '    })', '    cs := make(calls, 0, len(m))', '    for k, v := range m {', '        cs = append(cs, &call{k, v})', '    }', '    sort.Sort(cs)', '    for i, c := range cs {', '        fmt.Printf("%-20s %4d\\n", c.expr, c.count)', '        if i == 9 {', '            break', '        }', '    }', '}', '', 'type call struct {', '    expr  string', '    count int', '}', 'type calls []*call', '', 'func (c calls) Len() int           { return len(c) }', 'func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }', 'func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }'];
  $src = mochi_join($srcLines, '
');
  $freq = [];
  $i = 0;
  $order = [];
  while ($i < strlen($src)) {
  $ch = substr($src, $i, $i + 1 - $i);
  if (($ch >= 'A' && $ch <= 'Z') || ($ch >= 'a' && $ch <= 'z') || $ch == '_') {
  $j = $i + 1;
  while ($j < strlen($src) && isAlphaNumDot(substr($src, $j, $j + 1 - $j))) {
  $j = $j + 1;
};
  $token = substr($src, $i, $j - $i);
  $k = $j;
  while ($k < strlen($src)) {
  $cc = substr($src, $k, $k + 1 - $k);
  if ($cc == ' ' || $cc == '\t' || $cc == '
' || $cc == '') {
  $k = $k + 1;
} else {
  break;
}
};
  if ($k < strlen($src) && substr($src, $k, $k + 1 - $k) == '(') {
  $p = $i - 1;
  while ($p >= 0 && (substr($src, $p, $p + 1 - $p) == ' ' || substr($src, $p, $p + 1 - $p) == '\t')) {
  $p = $p - 1;
};
  $skip = false;
  if ($p >= 3) {
  $before = substr($src, $p - 3, $p + 1 - ($p - 3));
  if ($before == 'func') {
  $skip = true;
};
};
  if (!$skip) {
  if (array_key_exists($token, $freq)) {
  $freq[$token] = $freq[$token] + 1;
} else {
  $freq[$token] = 1;
  $order = array_merge($order, [$token]);
};
};
};
  $i = $j;
} else {
  $i = $i + 1;
}
};
  $pairs = [];
  foreach ($order as $t) {
  $pairs = array_merge($pairs, [['expr' => $t, 'count' => $freq[$t]]]);
};
  $pairs = sortPairs($pairs);
  $idx = 0;
  while ($idx < count($pairs) && $idx < 10) {
  $p = $pairs[$idx];
  echo rtrim($p['expr'] . ' ' . _str($p['count'])), PHP_EOL;
  $idx = $idx + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
