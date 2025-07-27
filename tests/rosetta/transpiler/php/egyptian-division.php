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
  function egyptianDivide($dividend, $divisor) {
  if ($dividend < 0 || $divisor <= 0) {
  $panic('Invalid argument(s)');
}
  if ($dividend < $divisor) {
  return ['q' => 0, 'r' => $dividend];
}
  $powers = [1];
  $doublings = [$divisor];
  $doubling = $divisor * 2;
  while ($doubling <= $dividend) {
  $powers = array_merge($powers, [$powers[count($powers) - 1] * 2]);
  $doublings = array_merge($doublings, [$doubling]);
  $doubling = $doubling * 2;
};
  $ans = 0;
  $accum = 0;
  $i = count($doublings) - 1;
  while ($i >= 0) {
  if ($accum + $doublings[$i] <= $dividend) {
  $accum = $accum + $doublings[$i];
  $ans = $ans + $powers[$i];
  if ($accum == $dividend) {
  break;
};
}
  $i = $i - 1;
};
  return ['q' => $ans, 'r' => $dividend - $accum];
};
  function main() {
  $dividend = 580;
  $divisor = 34;
  $res = egyptianDivide($dividend, $divisor);
  echo rtrim(_str($dividend) . ' divided by ' . _str($divisor) . ' is ' . _str($res['q']) . ' with remainder ' . _str($res['r'])), PHP_EOL;
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
