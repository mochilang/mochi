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
  function int_sqrt($n) {
  $r = 0;
  while (($r + 1) * ($r + 1) <= $n) {
  $r = $r + 1;
};
  return $r;
};
  function is_pronic($n) {
  if ($n < 0) {
  return false;
}
  if ($n % 2 != 0) {
  return false;
}
  $root = int_sqrt($n);
  return $n == $root * ($root + 1);
};
  function test_is_pronic() {
  if (is_pronic(-1)) {
  $panic('-1 should not be pronic');
}
  if (!is_pronic(0)) {
  $panic('0 should be pronic');
}
  if (!is_pronic(2)) {
  $panic('2 should be pronic');
}
  if (is_pronic(5)) {
  $panic('5 should not be pronic');
}
  if (!is_pronic(6)) {
  $panic('6 should be pronic');
}
  if (is_pronic(8)) {
  $panic('8 should not be pronic');
}
  if (!is_pronic(30)) {
  $panic('30 should be pronic');
}
  if (is_pronic(32)) {
  $panic('32 should not be pronic');
}
  if (!is_pronic(2147441940)) {
  $panic('2147441940 should be pronic');
}
};
  function main() {
  test_is_pronic();
  echo rtrim(json_encode(is_pronic(56), 1344)), PHP_EOL;
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
