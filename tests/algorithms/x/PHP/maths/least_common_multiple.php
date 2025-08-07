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
  function gcd($a, $b) {
  $x = ($a >= 0 ? $a : -$a);
  $y = ($b >= 0 ? $b : -$b);
  while ($y != 0) {
  $temp = $x % $y;
  $x = $y;
  $y = $temp;
};
  return $x;
};
  function lcm_slow($a, $b) {
  $max = ($a >= $b ? $a : $b);
  $multiple = $max;
  while (($multiple % $a != 0) || ($multiple % $b != 0)) {
  $multiple = $multiple + $max;
};
  return $multiple;
};
  function lcm_fast($a, $b) {
  return ($a / gcd($a, $b)) * $b;
};
  echo rtrim(_str(lcm_slow(5, 2))), PHP_EOL;
  echo rtrim(_str(lcm_slow(12, 76))), PHP_EOL;
  echo rtrim(_str(lcm_fast(5, 2))), PHP_EOL;
  echo rtrim(_str(lcm_fast(12, 76))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
