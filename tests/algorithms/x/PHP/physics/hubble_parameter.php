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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_pow($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function sqrt_approx($x) {
  if ($x == 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function hubble_parameter($hubble_constant, $radiation_density, $matter_density, $dark_energy, $redshift) {
  $parameters = [$redshift, $radiation_density, $matter_density, $dark_energy];
  $i = 0;
  while ($i < count($parameters)) {
  if ($parameters[$i] < 0.0) {
  _panic('All input parameters must be positive');
}
  $i = $i + 1;
};
  $i = 1;
  while ($i < 4) {
  if ($parameters[$i] > 1.0) {
  _panic('Relative densities cannot be greater than one');
}
  $i = $i + 1;
};
  $curvature = 1.0 - ($matter_density + $radiation_density + $dark_energy);
  $zp1 = $redshift + 1.0;
  $e2 = $radiation_density * mochi_pow($zp1, 4) + $matter_density * mochi_pow($zp1, 3) + $curvature * mochi_pow($zp1, 2) + $dark_energy;
  return $hubble_constant * sqrt_approx($e2);
};
  function test_hubble_parameter() {
  $h = hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0.0);
  if ($h < 68.2999 || $h > 68.3001) {
  _panic('hubble_parameter test failed');
}
};
  function main() {
  test_hubble_parameter();
  echo rtrim(json_encode(hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0.0), 1344)), PHP_EOL;
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
