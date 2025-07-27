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
  function contains($xs, $v) {
  foreach ($xs as $x) {
  if ($x == $v) {
  return true;
}
};
  return false;
};
  function copyInts($xs) {
  $out = [];
  foreach ($xs as $x) {
  $out = array_merge($out, [$x]);
};
  return $out;
};
  function sliceEqual($a, $b) {
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function reverse(&$xs) {
  $i = 0;
  $j = count($xs) - 1;
  while ($i < $j) {
  $t = $xs[$i];
  $xs[$i] = $xs[$j];
  $xs[$j] = $t;
  $i = $i + 1;
  $j = $j - 1;
};
};
  function perimEqual($p1, $p2) {
  if (count($p1) != count($p2)) {
  return false;
}
  foreach ($p1 as $v) {
  if (!in_array($v, $p2)) {
  return false;
}
};
  $c = copyInts($p1);
  $r = 0;
  while ($r < 2) {
  $i = 0;
  while ($i < count($c)) {
  if (sliceEqual($c, $p2)) {
  return true;
}
  $t = $c[count($c) - 1];
  $j = count($c) - 1;
  while ($j > 0) {
  $c[$j] = $c[$j - 1];
  $j = $j - 1;
};
  $c[0] = $t;
  $i = $i + 1;
};
  reverse($c);
  $r = $r + 1;
};
  return false;
};
  function sortEdges($es) {
  $arr = $es;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  $a = $arr[$j];
  $b = $arr[$j + 1];
  if ($a['a'] > $b['a'] || ($a['a'] == $b['a'] && $a['b'] > $b['b'])) {
  $arr[$j] = $b;
  $arr[$j + 1] = $a;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function concat($a, $b) {
  $out = [];
  foreach ($a as $x) {
  $out = array_merge($out, [$x]);
};
  foreach ($b as $x) {
  $out = array_merge($out, [$x]);
};
  return $out;
};
  function faceToPerim($face) {
  $le = count($face);
  if ($le == 0) {
  return null;
}
  $edges = [];
  $i = 0;
  while ($i < $le) {
  $e = $face[$i];
  if ($e['b'] <= $e['a']) {
  return null;
}
  $edges = array_merge($edges, [$e]);
  $i = $i + 1;
};
  $edges = sortEdges($edges);
  $firstEdge = $edges[0];
  $perim = [$firstEdge['a'], $firstEdge['b']];
  $first = $firstEdge['a'];
  $last = $firstEdge['b'];
  $edges = array_slice($edges, 1, count($edges) - 1);
  $le = count($edges);
  $done = false;
  while ($le > 0 && (!$done)) {
  $idx = 0;
  $found = false;
  while ($idx < $le) {
  $e = $edges[$idx];
  if ($e['a'] == $last) {
  $perim = array_merge($perim, [$e['b']]);
  $last = $e['b'];
  $found = true;
} else {
  if ($e['b'] == $last) {
  $perim = array_merge($perim, [$e['a']]);
  $last = $e['a'];
  $found = true;
};
}
  if ($found) {
  $edges = concat(array_slice($edges, 0, $idx - 0), array_slice($edges, $idx + 1, count($edges) - ($idx + 1)));
  $le = $le - 1;
  if ($last == $first) {
  if ($le == 0) {
  $done = true;
} else {
  return null;
};
};
  break;
}
  $idx = $idx + 1;
};
  if (!$found) {
  return null;
}
};
  return array_slice($perim, 0, count($perim) - 1 - 0);
};
  function listStr($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function main() {
  echo rtrim('Perimeter format equality checks:'), PHP_EOL;
  echo rtrim('  Q == R is ' . _str(perimEqual([8, 1, 3], [1, 3, 8]))), PHP_EOL;
  echo rtrim('  U == V is ' . _str(perimEqual([18, 8, 14, 10, 12, 17, 19], [8, 14, 10, 12, 17, 19, 18]))), PHP_EOL;
  $e = [['a' => 7, 'b' => 11], ['a' => 1, 'b' => 11], ['a' => 1, 'b' => 7]];
  $f = [['a' => 11, 'b' => 23], ['a' => 1, 'b' => 17], ['a' => 17, 'b' => 23], ['a' => 1, 'b' => 11]];
  $g = [['a' => 8, 'b' => 14], ['a' => 17, 'b' => 19], ['a' => 10, 'b' => 12], ['a' => 10, 'b' => 14], ['a' => 12, 'b' => 17], ['a' => 8, 'b' => 18], ['a' => 18, 'b' => 19]];
  $h = [['a' => 1, 'b' => 3], ['a' => 9, 'b' => 11], ['a' => 3, 'b' => 11], ['a' => 1, 'b' => 11]];
  echo rtrim('
Edge to perimeter format translations:'), PHP_EOL;
  $faces = [$e, $f, $g, $h];
  $names = ['E', 'F', 'G', 'H'];
  $idx = 0;
  while ($idx < count($faces)) {
  $per = faceToPerim($faces[$idx]);
  if ($per == null) {
  echo rtrim('  ' . $names[$idx] . ' => Invalid edge format'), PHP_EOL;
} else {
  echo rtrim('  ' . $names[$idx] . ' => ' . listStr($per)), PHP_EOL;
}
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
