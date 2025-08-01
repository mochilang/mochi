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
  function main() {
  $INF = 1000000000;
  $n = 4;
  $dist = [];
  $next = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $nrow = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = array_merge($row, [0]);
} else {
  $row = array_merge($row, [$INF]);
}
  $nrow = array_merge($nrow, [0 - 1]);
  $j = $j + 1;
};
  $dist = array_merge($dist, [$row]);
  $next = array_merge($next, [$nrow]);
  $i = $i + 1;
};
  $dist[0][2] = -2;
  $next[0][2] = 2;
  $dist[2][3] = 2;
  $next[2][3] = 3;
  $dist[3][1] = -1;
  $next[3][1] = 1;
  $dist[1][0] = 4;
  $next[1][0] = 0;
  $dist[1][2] = 3;
  $next[1][2] = 2;
  $k = 0;
  while ($k < $n) {
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n) {
  if ($dist[$i][$k] < $INF && $dist[$k][$j] < $INF) {
  $alt = $dist[$i][$k] + $dist[$k][$j];
  if ($alt < $dist[$i][$j]) {
  $dist[$i][$j] = $alt;
  $next[$i][$j] = $next[$i][$k];
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $k = $k + 1;
};
  $path = null;
$path = function($u, $v) use (&$path, $INF, $n, $dist, $next, $i, $row, $nrow, $j, $k, $alt) {
  $ui = $u - 1;
  $vi = $v - 1;
  if ($next[$ui][$vi] == 0 - 1) {
  return [];
}
  $p = [$u];
  $cur = $ui;
  while ($cur != $vi) {
  $cur = $next[$cur][$vi];
  $p = array_merge($p, [$cur + 1]);
};
  return $p;
};
  $pathStr = null;
$pathStr = function($p) use (&$pathStr, $INF, $n, $dist, $next, $i, $row, $nrow, $j, $k, $alt, $path) {
  $s = '';
  $first = true;
  $idx = 0;
  while ($idx < count($p)) {
  $x = $p[$idx];
  if (!$first) {
  $s = $s . ' -> ';
}
  $s = $s . _str($x);
  $first = false;
  $idx = $idx + 1;
};
  return $s;
};
  echo rtrim('pair	dist	path'), PHP_EOL;
  $a = 0;
  while ($a < $n) {
  $b = 0;
  while ($b < $n) {
  if ($a != $b) {
  echo rtrim(_str($a + 1) . ' -> ' . _str($b + 1) . '	' . _str($dist[$a][$b]) . '	' . $pathStr($path($a + 1, $b + 1))), PHP_EOL;
}
  $b = $b + 1;
};
  $a = $a + 1;
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
