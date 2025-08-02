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
  function powf($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function normalize($v) {
  $len = sqrtApprox($v['x'] * $v['x'] + $v['y'] * $v['y'] + $v['z'] * $v['z']);
  return ['x' => $v['x'] / $len, 'y' => $v['y'] / $len, 'z' => $v['z'] / $len];
};
  function dot($a, $b) {
  $d = $a['x'] * $b['x'] + $a['y'] * $b['y'] + $a['z'] * $b['z'];
  if ($d < 0.0) {
  return -$d;
}
  return 0.0;
};
  function drawSphere($r, $k, $ambient, $light, $shades) {
  $i = -$r;
  while ($i <= $r) {
  $x = (floatval($i)) + 0.5;
  $line = '';
  $j = -(2 * $r);
  while ($j <= 2 * $r) {
  $y = (floatval($j)) / 2.0 + 0.5;
  if ($x * $x + $y * $y <= (floatval($r)) * (floatval($r))) {
  $zsq = (floatval($r)) * (floatval($r)) - $x * $x - $y * $y;
  $vec = normalize(['x' => $x, 'y' => $y, 'z' => sqrtApprox($zsq)]);
  $b = powf(dot($light, $vec), $k) + $ambient;
  $intensity = intval(((1.0 - $b) * ((floatval(strlen($shades))) - 1.0)));
  if ($intensity < 0) {
  $intensity = 0;
};
  if ($intensity >= strlen($shades)) {
  $intensity = strlen($shades) - 1;
};
  $line = $line . substr($shades, $intensity, $intensity + 1 - $intensity);
} else {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  function main() {
  $shades = '.:!*oe&#%@';
  $light = normalize(['x' => 30.0, 'y' => 30.0, 'z' => -50.0]);
  drawSphere(20, 4, 0.1, $light, $shades);
  drawSphere(10, 2, 0.4, $light, $shades);
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
