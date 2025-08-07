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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $lcg_seed = 1;
  function lcg_rand() {
  global $lcg_seed;
  $lcg_seed = ($lcg_seed * 1103515245 + 12345) % 2147483648;
  return $lcg_seed;
};
  function roll() {
  global $lcg_seed;
  $rv = floatval(lcg_rand());
  $r = $rv * 6.0 / 2147483648.0;
  return 1 + (intval($r));
};
  function round2($x) {
  global $lcg_seed;
  $y = $x * 100.0 + 0.5;
  $z = intval($y);
  return (floatval($z)) / 100.0;
};
  function throw_dice($num_throws, $num_dice) {
  global $lcg_seed;
  $count_of_sum = [];
  $max_sum = $num_dice * 6 + 1;
  $i = 0;
  while ($i < $max_sum) {
  $count_of_sum = _append($count_of_sum, 0);
  $i = $i + 1;
};
  $t = 0;
  while ($t < $num_throws) {
  $s = 0;
  $d = 0;
  while ($d < $num_dice) {
  $s = $s + roll();
  $d = $d + 1;
};
  $count_of_sum[$s] = $count_of_sum[$s] + 1;
  $t = $t + 1;
};
  $probability = [];
  $i = $num_dice;
  while ($i < $max_sum) {
  $p = (floatval($count_of_sum[$i])) * 100.0 / (floatval($num_throws));
  $probability = _append($probability, round2($p));
  $i = $i + 1;
};
  return $probability;
};
  function main() {
  global $lcg_seed;
  $lcg_seed = 1;
  $result = throw_dice(10000, 2);
  echo rtrim(_str($result)), PHP_EOL;
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
