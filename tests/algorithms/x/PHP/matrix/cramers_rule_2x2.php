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
  function cramers_rule_2x2($eq1, $eq2) {
  if (count($eq1) != 3 || count($eq2) != 3) {
  _panic('Please enter a valid equation.');
}
  if ($eq1[0] == 0.0 && $eq1[1] == 0.0 && $eq2[0] == 0.0 && $eq2[1] == 0.0) {
  _panic('Both a & b of two equations can\'t be zero.');
}
  $a1 = $eq1[0];
  $b1 = $eq1[1];
  $c1 = $eq1[2];
  $a2 = $eq2[0];
  $b2 = $eq2[1];
  $c2 = $eq2[2];
  $determinant = $a1 * $b2 - $a2 * $b1;
  $determinant_x = $c1 * $b2 - $c2 * $b1;
  $determinant_y = $a1 * $c2 - $a2 * $c1;
  if ($determinant == 0.0) {
  if ($determinant_x == 0.0 && $determinant_y == 0.0) {
  _panic('Infinite solutions. (Consistent system)');
};
  _panic('No solution. (Inconsistent system)');
}
  if ($determinant_x == 0.0 && $determinant_y == 0.0) {
  return [0.0, 0.0];
}
  $x = $determinant_x / $determinant;
  $y = $determinant_y / $determinant;
  return [$x, $y];
};
  function test_cramers_rule_2x2() {
  $r1 = cramers_rule_2x2([2.0, 3.0, 0.0], [5.0, 1.0, 0.0]);
  if ($r1[0] != 0.0 || $r1[1] != 0.0) {
  _panic('Test1 failed');
}
  $r2 = cramers_rule_2x2([0.0, 4.0, 50.0], [2.0, 0.0, 26.0]);
  if ($r2[0] != 13.0 || $r2[1] != 12.5) {
  _panic('Test2 failed');
}
};
  function main() {
  test_cramers_rule_2x2();
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(cramers_rule_2x2([11.0, 2.0, 30.0], [1.0, 0.0, 4.0]), 1344)))))), PHP_EOL;
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
