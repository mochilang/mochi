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
  function parseBool($s) {
  $l = strtolower($s);
  if ($l == '1' || $l == 't' || $l == true || $l == 'yes' || $l == 'y') {
  return true;
}
  return false;
};
  function main() {
  $n = true;
  echo rtrim(($n ? 'true' : 'false')), PHP_EOL;
  echo rtrim('bool'), PHP_EOL;
  $n = !$n;
  echo rtrim(($n ? 'true' : 'false')), PHP_EOL;
  $x = 5;
  $y = 8;
  echo rtrim('x == y:') . " " . rtrim(($x == $y ? 'true' : 'false')), PHP_EOL;
  echo rtrim('x < y:') . " " . rtrim(($x < $y ? 'true' : 'false')), PHP_EOL;
  echo rtrim('\nConvert String into Boolean Data type\n'), PHP_EOL;
  $str1 = 'japan';
  echo rtrim('Before :') . " " . rtrim('string'), PHP_EOL;
  $bolStr = parseBool($str1);
  echo rtrim('After :') . " " . rtrim('bool'), PHP_EOL;
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
