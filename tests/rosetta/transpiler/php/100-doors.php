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
  $doors = [];
  for ($i = 0; $i < 100; $i++) {
  $doors = array_merge($doors, [false]);
}
  for ($pass = 1; $pass < 101; $pass++) {
  $idx = $pass - 1;
  while ($idx < 100) {
  $doors[$idx] = !$doors[$idx];
  $idx = $idx + $pass;
};
}
  for ($row = 0; $row < 10; $row++) {
  $line = '';
  for ($col = 0; $col < 10; $col++) {
  $idx = $row * 10 + $col;
  if ($doors[$idx]) {
  $line = $line . '1';
} else {
  $line = $line . '0';
}
  if ($col < 9) {
  $line = $line . ' ';
}
};
  echo rtrim($line), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
