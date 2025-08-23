<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  function mochi_contains($chars, $ch) {
  $i = 0;
  while ($i < strlen($chars)) {
  if (substr($chars, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function mochi_substring($s, $start, $end) {
  $res = '';
  $i = $start;
  while ($i < $end) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
};
  return $res;
};
  function strip_chars($user_string, $characters) {
  $start = 0;
  $end = strlen($user_string);
  while ($start < $end && mochi_contains($characters, substr($user_string, $start, $start + 1 - $start))) {
  $start = $start + 1;
};
  while ($end > $start && mochi_contains($characters, substr($user_string, $end - 1, $end - 1 + 1 - ($end - 1)))) {
  $end = $end - 1;
};
  return substr($user_string, $start, $end - $start);
};
  function strip($user_string) {
  return strip_chars($user_string, ' 	
');
};
  function test_strip() {
  if (strip('   hello   ') != 'hello') {
  _panic('test1 failed');
}
  if (strip_chars('...world...', '.') != 'world') {
  _panic('test2 failed');
}
  if (strip_chars('123hello123', '123') != 'hello') {
  _panic('test3 failed');
}
  if (strip('') != '') {
  _panic('test4 failed');
}
};
  function main() {
  test_strip();
  echo rtrim(strip('   hello   ')), PHP_EOL;
  echo rtrim(strip_chars('...world...', '.')), PHP_EOL;
  echo rtrim(strip_chars('123hello123', '123')), PHP_EOL;
  echo rtrim(strip('')), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
