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
  function sqrtApprox($x) {
  if ($x <= 0.0) {
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
  function electrical_impedance($resistance, $reactance, $impedance) {
  $zero_count = 0;
  if ($resistance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($reactance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($impedance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  _panic('One and only one argument must be 0');
}
  if ($resistance == 0.0) {
  $value = sqrtApprox($impedance * $impedance - $reactance * $reactance);
  return ['resistance' => $value];
} else {
  if ($reactance == 0.0) {
  $value = sqrtApprox($impedance * $impedance - $resistance * $resistance);
  return ['reactance' => $value];
} else {
  if ($impedance == 0.0) {
  $value = sqrtApprox($resistance * $resistance + $reactance * $reactance);
  return ['impedance' => $value];
} else {
  _panic('Exactly one argument must be 0');
};
};
}
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(electrical_impedance(3.0, 4.0, 0.0), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(electrical_impedance(0.0, 4.0, 5.0), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(electrical_impedance(3.0, 0.0, 5.0), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
