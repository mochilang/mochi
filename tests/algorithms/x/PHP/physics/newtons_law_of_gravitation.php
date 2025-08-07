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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $GRAVITATIONAL_CONSTANT = 0.000000000066743;
  function sqrtApprox($x) {
  global $GRAVITATIONAL_CONSTANT, $r1, $r2, $r3, $r4;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function gravitational_law($force, $mass_1, $mass_2, $distance) {
  global $GRAVITATIONAL_CONSTANT, $r1, $r2, $r3, $r4;
  $zero_count = 0;
  if ($force == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($mass_1 == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($mass_2 == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($distance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  _panic('One and only one argument must be 0');
}
  if ($force < 0.0) {
  _panic('Gravitational force can not be negative');
}
  if ($distance < 0.0) {
  _panic('Distance can not be negative');
}
  if ($mass_1 < 0.0) {
  _panic('Mass can not be negative');
}
  if ($mass_2 < 0.0) {
  _panic('Mass can not be negative');
}
  $product_of_mass = $mass_1 * $mass_2;
  if ($force == 0.0) {
  $f = $GRAVITATIONAL_CONSTANT * $product_of_mass / ($distance * $distance);
  return ['kind' => 'force', 'value' => $f];
}
  if ($mass_1 == 0.0) {
  $m1 = $force * ($distance * $distance) / ($GRAVITATIONAL_CONSTANT * $mass_2);
  return ['kind' => 'mass_1', 'value' => $m1];
}
  if ($mass_2 == 0.0) {
  $m2 = $force * ($distance * $distance) / ($GRAVITATIONAL_CONSTANT * $mass_1);
  return ['kind' => 'mass_2', 'value' => $m2];
}
  $d = sqrtApprox($GRAVITATIONAL_CONSTANT * $product_of_mass / $force);
  return ['kind' => 'distance', 'value' => $d];
};
  $r1 = gravitational_law(0.0, 5.0, 10.0, 20.0);
  $r2 = gravitational_law(7367.382, 0.0, 74.0, 3048.0);
  $r3 = gravitational_law(100.0, 5.0, 0.0, 3.0);
  $r4 = gravitational_law(100.0, 5.0, 10.0, 0.0);
  echo rtrim($r1['kind'] . ' ' . _str($r1['value'])), PHP_EOL;
  echo rtrim($r2['kind'] . ' ' . _str($r2['value'])), PHP_EOL;
  echo rtrim($r3['kind'] . ' ' . _str($r3['value'])), PHP_EOL;
  echo rtrim($r4['kind'] . ' ' . _str($r4['value'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
