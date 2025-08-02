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
  function mapString($s, $f) {
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $out = $out . $f(substr($s, $i, $i + 1 - $i));
  $i = $i + 1;
};
  return $out;
};
  function main() {
  $fn = null;
$fn = function($r) use ($fn) {
  return ($r == ' ' ? '' : $r);
};
  mapString('Spaces removed', $fn);
  mapString('Test', function($r) use ($fn, $strtolower) {
  return strtolower($r);
});
  mapString('shift', function($r) use ($fn) {
  return $r;
});
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
