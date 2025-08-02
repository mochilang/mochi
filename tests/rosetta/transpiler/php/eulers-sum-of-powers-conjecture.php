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
  function eulerSum() {
  $pow5 = [];
  $i = 0;
  while ($i < 250) {
  $pow5 = array_merge($pow5, [$i * $i * $i * $i * $i]);
  $i = $i + 1;
};
  $sums = [];
  $x2 = 2;
  while ($x2 < 250) {
  $x3 = 1;
  while ($x3 < $x2) {
  $s = $pow5[$x2] + $pow5[$x3];
  if (!(array_key_exists($s, $sums))) {
  $sums[$s] = [$x2, $x3];
}
  $x3 = $x3 + 1;
};
  $x2 = $x2 + 1;
};
  $x0 = 4;
  while ($x0 < 250) {
  $x1 = 3;
  while ($x1 < $x0) {
  $y = $x0 + 1;
  while ($y < 250) {
  $rem = $pow5[$y] - $pow5[$x0] - $pow5[$x1];
  if (array_key_exists($rem, $sums)) {
  $pair = $sums[$rem];
  $a = $pair[0];
  $b = $pair[1];
  if ($x1 > $a && $a > $b) {
  return [$x0, $x1, $a, $b, $y];
};
}
  $y = $y + 1;
};
  $x1 = $x1 + 1;
};
  $x0 = $x0 + 1;
};
  return [0, 0, 0, 0, 0];
};
  function main() {
  $r = eulerSum();
  echo rtrim(_str($r[0]) . ' ' . _str($r[1]) . ' ' . _str($r[2]) . ' ' . _str($r[3]) . ' ' . _str($r[4])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
