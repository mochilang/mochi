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
  function make_body($px, $py, $vx, $vy, $mass) {
  return ['position_x' => $px, 'position_y' => $py, 'velocity_x' => $vx, 'velocity_y' => $vy, 'mass' => $mass];
};
  function update_velocity(&$body, $force_x, $force_y, $delta_time) {
  $body['velocity_x'] = $body['velocity_x'] + $force_x * $delta_time;
  $body['velocity_y'] = $body['velocity_y'] + $force_y * $delta_time;
  return $body;
};
  function update_position(&$body, $delta_time) {
  $body['position_x'] = $body['position_x'] + $body['velocity_x'] * $delta_time;
  $body['position_y'] = $body['position_y'] + $body['velocity_y'] * $delta_time;
  return $body;
};
  function make_body_system($bodies, $g, $tf, $sf) {
  return ['bodies' => $bodies, 'gravitation_constant' => $g, 'time_factor' => $tf, 'softening_factor' => $sf];
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
  function update_system(&$system, $delta_time) {
  $bodies = $system['bodies'];
  $i = 0;
  while ($i < count($bodies)) {
  $body1 = $bodies[$i];
  $force_x = 0.0;
  $force_y = 0.0;
  $j = 0;
  while ($j < count($bodies)) {
  if ($i != $j) {
  $body2 = $bodies[$j];
  $dif_x = $body2['position_x'] - $body1['position_x'];
  $dif_y = $body2['position_y'] - $body1['position_y'];
  $distance_sq = $dif_x * $dif_x + $dif_y * $dif_y + $system['softening_factor'];
  $distance = sqrtApprox($distance_sq);
  $denom = $distance * $distance * $distance;
  $force_x = $force_x + $system['gravitation_constant'] * $body2['mass'] * $dif_x / $denom;
  $force_y = $force_y + $system['gravitation_constant'] * $body2['mass'] * $dif_y / $denom;
}
  $j = $j + 1;
};
  $body1 = update_velocity($body1, $force_x, $force_y, $delta_time * $system['time_factor']);
  $bodies[$i] = $body1;
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($bodies)) {
  $body = $bodies[$i];
  $body = update_position($body, $delta_time * $system['time_factor']);
  $bodies[$i] = $body;
  $i = $i + 1;
};
  $system['bodies'] = $bodies;
  return $system;
};
  function main() {
  $b1 = make_body(0.0, 0.0, 0.0, 0.0, 1.0);
  $b2 = make_body(10.0, 0.0, 0.0, 0.0, 1.0);
  $sys1 = make_body_system([$b1, $b2], 1.0, 1.0, 0.0);
  $sys1 = update_system($sys1, 1.0);
  $b1_after = $sys1['bodies'][0];
  $pos1x = $b1_after['position_x'];
  $pos1y = $b1_after['position_y'];
  echo str_replace('    ', '  ', json_encode(['x' => $pos1x, 'y' => $pos1y], 128)), PHP_EOL;
  $vel1x = $b1_after['velocity_x'];
  $vel1y = $b1_after['velocity_y'];
  echo str_replace('    ', '  ', json_encode(['vx' => $vel1x, 'vy' => $vel1y], 128)), PHP_EOL;
  $b3 = make_body(-10.0, 0.0, 0.0, 0.0, 1.0);
  $b4 = make_body(10.0, 0.0, 0.0, 0.0, 4.0);
  $sys2 = make_body_system([$b3, $b4], 1.0, 10.0, 0.0);
  $sys2 = update_system($sys2, 1.0);
  $b2_after = $sys2['bodies'][0];
  $pos2x = $b2_after['position_x'];
  $pos2y = $b2_after['position_y'];
  echo str_replace('    ', '  ', json_encode(['x' => $pos2x, 'y' => $pos2y], 128)), PHP_EOL;
  $vel2x = $b2_after['velocity_x'];
  $vel2y = $b2_after['velocity_y'];
  echo str_replace('    ', '  ', json_encode(['vx' => $vel2x, 'vy' => $vel2y], 128)), PHP_EOL;
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
