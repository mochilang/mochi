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
  $PI = 3.141592653589793;
  $REDUCED_PLANCK_CONSTANT = 0.0000000000000000000000000000000001054571817;
  $SPEED_OF_LIGHT = 300000000.0;
  function sqrtApprox($x) {
  global $PI, $REDUCED_PLANCK_CONSTANT, $SPEED_OF_LIGHT;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 100) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function casimir_force($force, $area, $distance) {
  global $PI, $REDUCED_PLANCK_CONSTANT, $SPEED_OF_LIGHT;
  $zero_count = 0;
  if ($force == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($area == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($distance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  _panic('One and only one argument must be 0');
}
  if ($force < 0.0) {
  _panic('Magnitude of force can not be negative');
}
  if ($distance < 0.0) {
  _panic('Distance can not be negative');
}
  if ($area < 0.0) {
  _panic('Area can not be negative');
}
  if ($force == 0.0) {
  $num = $REDUCED_PLANCK_CONSTANT * $SPEED_OF_LIGHT * $PI * $PI * $area;
  $den = 240.0 * $distance * $distance * $distance * $distance;
  $f = $num / $den;
  return ['force' => $f];
}
  if ($area == 0.0) {
  $num = 240.0 * $force * $distance * $distance * $distance * $distance;
  $den = $REDUCED_PLANCK_CONSTANT * $SPEED_OF_LIGHT * $PI * $PI;
  $a = $num / $den;
  return ['area' => $a];
}
  $num = $REDUCED_PLANCK_CONSTANT * $SPEED_OF_LIGHT * $PI * $PI * $area;
  $den = 240.0 * $force;
  $inner = $num / $den;
  $d = sqrtApprox(sqrtApprox($inner));
  return ['distance' => $d];
};
  function main() {
  global $PI, $REDUCED_PLANCK_CONSTANT, $SPEED_OF_LIGHT;
  echo rtrim(_str(casimir_force(0.0, 4.0, 0.03))), PHP_EOL;
  echo rtrim(_str(casimir_force(0.0000000002635, 0.0023, 0.0))), PHP_EOL;
  echo rtrim(_str(casimir_force(0.000000000000000002737, 0.0, 0.0023746))), PHP_EOL;
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
