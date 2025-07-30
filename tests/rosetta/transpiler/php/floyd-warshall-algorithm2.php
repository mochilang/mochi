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
  $INF = 1000000;
  function floydWarshall($graph) {
  global $INF, $g, $row, $res, $p;
  $n = count($graph);
  $dist = [];
  $next = [];
  $i = 0;
  while ($i < $n) {
  $drow = [];
  $nrow = [];
  $j = 0;
  while ($j < $n) {
  $drow = array_merge($drow, [$graph[$i][$j]]);
  if ($graph[$i][$j] < $INF && $i != $j) {
  $nrow = array_merge($nrow, [$j]);
} else {
  $nrow = array_merge($nrow, [-1]);
}
  $j = $j + 1;
};
  $dist = array_merge($dist, [$drow]);
  $next = array_merge($next, [$nrow]);
  $i = $i + 1;
};
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
  return ['dist' => $dist, 'next' => $next];
};
  function path($u, $v, $next) {
  global $INF, $n, $g, $row, $res, $i, $j;
  if ($next[$u][$v] < 0) {
  return [];
}
  $p = [$u];
  $x = $u;
  while ($x != $v) {
  $x = $next[$x][$v];
  $p = array_merge($p, [$x]);
};
  return $p;
};
  function pathStr($p) {
  global $INF, $n, $g, $row, $res, $j;
  $s = '';
  $i = 0;
  while ($i < count($p)) {
  $s = $s . _str($p[$i] + 1);
  if ($i < count($p) - 1) {
  $s = $s . ' -> ';
}
  $i = $i + 1;
};
  return $s;
};
  $n = 4;
  $g = [];
  for ($i = 0; $i < $n; $i++) {
  $row = [];
  for ($j = 0; $j < $n; $j++) {
  if ($i == $j) {
  $row = array_merge($row, [0]);
} else {
  $row = array_merge($row, [$INF]);
}
};
  $g = array_merge($g, [$row]);
}
  $g[0][2] = -2;
  $g[2][3] = 2;
  $g[3][1] = -1;
  $g[1][0] = 4;
  $g[1][2] = 3;
  $res = floydWarshall($g);
  echo rtrim('pair\tdist\tpath'), PHP_EOL;
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n) {
  if ($i != $j) {
  $p = path($i, $j, $res['next']);
  echo rtrim(_str($i + 1) . ' -> ' . _str($j + 1) . '\t' . _str($res['dist'][$i][$j]) . '\t' . pathStr($p)), PHP_EOL;
}
  $j = $j + 1;
};
  $i = $i + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
