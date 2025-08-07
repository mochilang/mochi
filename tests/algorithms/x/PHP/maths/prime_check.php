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
  function is_prime($number) {
  if ($number < 0) {
  $panic('is_prime() only accepts positive integers');
}
  if ((1 < $number) && ($number < 4)) {
  return true;
} else {
  if (($number < 2) || ($number % 2 == 0) || ($number % 3 == 0)) {
  return false;
};
}
  $i = 5;
  while ($i * $i <= $number) {
  if (($number % $i == 0) || ($number % ($i + 2) == 0)) {
  return false;
}
  $i = $i + 6;
};
  return true;
};
  echo rtrim(_str(is_prime(0))), PHP_EOL;
  echo rtrim(_str(is_prime(1))), PHP_EOL;
  echo rtrim(_str(is_prime(2))), PHP_EOL;
  echo rtrim(_str(is_prime(3))), PHP_EOL;
  echo rtrim(_str(is_prime(27))), PHP_EOL;
  echo rtrim(_str(is_prime(87))), PHP_EOL;
  echo rtrim(_str(is_prime(563))), PHP_EOL;
  echo rtrim(_str(is_prime(2999))), PHP_EOL;
  echo rtrim(_str(is_prime(67483))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
