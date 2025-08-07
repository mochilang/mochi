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
  function doppler_effect($org_freq, $wave_vel, $obs_vel, $src_vel) {
  if ($wave_vel == $src_vel) {
  _panic('division by zero implies vs=v and observer in front of the source');
}
  $doppler_freq = ($org_freq * ($wave_vel + $obs_vel)) / ($wave_vel - $src_vel);
  if ($doppler_freq <= 0.0) {
  _panic('non-positive frequency implies vs>v or v0>v (in the opposite direction)');
}
  return $doppler_freq;
};
  function absf($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function almost_equal($a, $b, $tol) {
  return absf($a - $b) <= $tol;
};
  function test_doppler_effect() {
  if (!almost_equal(doppler_effect(100.0, 330.0, 10.0, 0.0), 103.03030303030303, 0.0000001)) {
  _panic('test 1 failed');
}
  if (!almost_equal(doppler_effect(100.0, 330.0, -10.0, 0.0), 96.96969696969697, 0.0000001)) {
  _panic('test 2 failed');
}
  if (!almost_equal(doppler_effect(100.0, 330.0, 0.0, 10.0), 103.125, 0.0000001)) {
  _panic('test 3 failed');
}
  if (!almost_equal(doppler_effect(100.0, 330.0, 0.0, -10.0), 97.05882352941177, 0.0000001)) {
  _panic('test 4 failed');
}
  if (!almost_equal(doppler_effect(100.0, 330.0, 10.0, 10.0), 106.25, 0.0000001)) {
  _panic('test 5 failed');
}
  if (!almost_equal(doppler_effect(100.0, 330.0, -10.0, -10.0), 94.11764705882354, 0.0000001)) {
  _panic('test 6 failed');
}
};
  function main() {
  test_doppler_effect();
  echo rtrim(json_encode(doppler_effect(100.0, 330.0, 10.0, 0.0), 1344)), PHP_EOL;
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
