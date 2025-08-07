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
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function euclidean_distance($v1, $v2) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($v1)) {
  $diff = $v1[$i] - $v2[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return sqrtApprox($sum);
};
  function euclidean_distance_no_np($v1, $v2) {
  return euclidean_distance($v1, $v2);
};
  function main() {
  echo rtrim(_str(euclidean_distance([0.0, 0.0], [2.0, 2.0]))), PHP_EOL;
  echo rtrim(_str(euclidean_distance([0.0, 0.0, 0.0], [2.0, 2.0, 2.0]))), PHP_EOL;
  echo rtrim(_str(euclidean_distance([1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0]))), PHP_EOL;
  echo rtrim(_str(euclidean_distance_no_np([1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0]))), PHP_EOL;
  echo rtrim(_str(euclidean_distance_no_np([0.0, 0.0], [2.0, 2.0]))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
