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
  function move_tower($height, $from_pole, $to_pole, $with_pole) {
  if ($height >= 1) {
  move_tower($height - 1, $from_pole, $with_pole, $to_pole);
  move_disk($from_pole, $to_pole);
  move_tower($height - 1, $with_pole, $to_pole, $from_pole);
}
};
  function move_disk($fp, $tp) {
  global $height;
  echo rtrim('moving disk from ' . $fp . ' to ' . $tp), PHP_EOL;
};
  $height = 3;
  move_tower($height, 'A', 'B', 'C');
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
