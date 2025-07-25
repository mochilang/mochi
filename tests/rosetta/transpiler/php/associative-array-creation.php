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
  function removeKey($m, $k) {
  $out = [];
  foreach (array_keys($m) as $key) {
  if ($key != $k) {
  $out[$key] = $m[$key];
}
};
  return $out;
};
  function main() {
  $x = null;
  $x = [];
  $x['foo'] = 3;
  $y1 = $x['bar'];
  $ok = array_key_exists('bar', $x);
  echo rtrim(json_encode($y1, 1344)), PHP_EOL;
  echo rtrim(($ok ? 'true' : 'false')), PHP_EOL;
  $x = removeKey($x, 'foo');
  $x = ['foo' => 2, 'bar' => 42, 'baz' => -1];
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($x['foo'], 1344))))))) . " " . rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($x['bar'], 1344))))))) . " " . rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($x['baz'], 1344))))))), PHP_EOL;
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
