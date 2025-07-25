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
  function push(&$h, $it) {
  $h = array_merge($h, [$it]);
  $i = count($h) - 1;
  while ($i > 0 && $h[$i - 1]['s'] > $h[$i]['s']) {
  $tmp = $h[$i - 1];
  $h[$i - 1] = $h[$i];
  $h[$i] = $tmp;
  $i = $i - 1;
};
  return $h;
};
  function step($h, $nv, $dir) {
  while (count($h) == 0 || $nv * $nv <= $h[0]['s']) {
  $h = push($h, ['s' => $nv * $nv, 'a' => $nv, 'b' => 0]);
  $nv = $nv + 1;
};
  $s = $h[0]['s'];
  $v = [];
  while (count($h) > 0 && $h[0]['s'] == $s) {
  $it = $h[0];
  $h = array_slice($h, 1);
  $v = array_merge($v, [[$it['a'], $it['b']]]);
  if ($it['a'] > $it['b']) {
  $h = push($h, ['s' => $it['a'] * $it['a'] + ($it['b'] + 1) * ($it['b'] + 1), 'a' => $it['a'], 'b' => $it['b'] + 1]);
}
};
  $list = [];
  foreach ($v as $p) {
  $list = array_merge($list, [$p]);
};
  $temp = $list;
  foreach ($temp as $p) {
  if ($p[0] != $p[1]) {
  $list = array_merge($list, [[$p[1], $p[0]]]);
}
};
  $temp = $list;
  foreach ($temp as $p) {
  if ($p[1] != 0) {
  $list = array_merge($list, [[$p[0], -$p[1]]]);
}
};
  $temp = $list;
  foreach ($temp as $p) {
  if ($p[0] != 0) {
  $list = array_merge($list, [[-$p[0], $p[1]]]);
}
};
  $bestDot = -999999999;
  $best = $dir;
  foreach ($list as $p) {
  $cross = $p[0] * $dir[1] - $p[1] * $dir[0];
  if ($cross >= 0) {
  $dot = $p[0] * $dir[0] + $p[1] * $dir[1];
  if ($dot > $bestDot) {
  $bestDot = $dot;
  $best = $p;
};
}
};
  return ['d' => $best, 'heap' => $h, 'n' => $nv];
};
  function positions($n) {
  $pos = [];
  $x = 0;
  $y = 0;
  $dir = [0, 1];
  $heap = [];
  $nv = 1;
  $i = 0;
  while ($i < $n) {
  $pos = array_merge($pos, [[$x, $y]]);
  $st = step($heap, $nv, $dir);
  $dir = $st['d'];
  $heap = $st['heap'];
  $nv = ord($st['n']);
  $x = $x + $dir[0];
  $y = $y + $dir[1];
  $i = $i + 1;
};
  return $pos;
};
  function pad($s, $w) {
  $r = $s;
  while (strlen($r) < $w) {
  $r = $r . ' ';
};
  return $r;
};
  function main() {
  $pts = positions(40);
  echo rtrim('The first 40 Babylonian spiral points are:'), PHP_EOL;
  $line = '';
  $i = 0;
  while ($i < count($pts)) {
  $p = $pts[$i];
  $s = pad('(' . _str($p[0]) . ', ' . _str($p[1]) . ')', 10);
  $line = $line . $s;
  if (($i + 1) % 10 == 0) {
  echo rtrim($line), PHP_EOL;
  $line = '';
}
  $i = $i + 1;
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
