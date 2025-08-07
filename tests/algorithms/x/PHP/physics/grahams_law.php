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
$__start_mem = memory_get_usage();
$__start = _now();
  function to_float($x) {
  return $x * 1.0;
};
  function round6($x) {
  $factor = 1000000.0;
  return to_float(intval($x * $factor + 0.5)) / $factor;
};
  function sqrtApprox($x) {
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function validate($values) {
  if (count($values) == 0) {
  return false;
}
  $i = 0;
  while ($i < count($values)) {
  if ($values[$i] <= 0.0) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function effusion_ratio($m1, $m2) {
  if (!validate([$m1, $m2])) {
  echo rtrim('ValueError: Molar mass values must greater than 0.'), PHP_EOL;
  return 0.0;
}
  return round6(sqrtApprox($m2 / $m1));
};
  function first_effusion_rate($rate, $m1, $m2) {
  if (!validate([$rate, $m1, $m2])) {
  echo rtrim('ValueError: Molar mass and effusion rate values must greater than 0.'), PHP_EOL;
  return 0.0;
}
  return round6($rate * sqrtApprox($m2 / $m1));
};
  function second_effusion_rate($rate, $m1, $m2) {
  if (!validate([$rate, $m1, $m2])) {
  echo rtrim('ValueError: Molar mass and effusion rate values must greater than 0.'), PHP_EOL;
  return 0.0;
}
  return round6($rate / sqrtApprox($m2 / $m1));
};
  function first_molar_mass($mass, $r1, $r2) {
  if (!validate([$mass, $r1, $r2])) {
  echo rtrim('ValueError: Molar mass and effusion rate values must greater than 0.'), PHP_EOL;
  return 0.0;
}
  $ratio = $r1 / $r2;
  return round6($mass / ($ratio * $ratio));
};
  function second_molar_mass($mass, $r1, $r2) {
  if (!validate([$mass, $r1, $r2])) {
  echo rtrim('ValueError: Molar mass and effusion rate values must greater than 0.'), PHP_EOL;
  return 0.0;
}
  $ratio = $r1 / $r2;
  return round6(($ratio * $ratio) / $mass);
};
  echo rtrim(json_encode(effusion_ratio(2.016, 4.002), 1344)), PHP_EOL;
  echo rtrim(json_encode(first_effusion_rate(1.0, 2.016, 4.002), 1344)), PHP_EOL;
  echo rtrim(json_encode(second_effusion_rate(1.0, 2.016, 4.002), 1344)), PHP_EOL;
  echo rtrim(json_encode(first_molar_mass(2.0, 1.408943, 0.709752), 1344)), PHP_EOL;
  echo rtrim(json_encode(second_molar_mass(2.0, 1.408943, 0.709752), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
