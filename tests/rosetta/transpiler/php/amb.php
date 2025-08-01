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
  function amb($wordsets, &$res, $idx) {
  if ($idx == count($wordsets)) {
  return true;
}
  $prev = '';
  if ($idx > 0) {
  $prev = $res[$idx - 1];
}
  $i = 0;
  while ($i < count($wordsets[$idx])) {
  $w = $wordsets[$idx][$i];
  if ($idx == 0 || substr($prev, strlen($prev) - 1, strlen($prev) - (strlen($prev) - 1)) == substr($w, 0, 1 - 0)) {
  $res[$idx] = $w;
  if (amb($wordsets, $res, $idx + 1)) {
  return true;
};
}
  $i = $i + 1;
};
  return false;
};
  function main() {
  $wordset = [['the', 'that', 'a'], ['frog', 'elephant', 'thing'], ['walked', 'treaded', 'grows'], ['slowly', 'quickly']];
  $res = [];
  $i = 0;
  while ($i < count($wordset)) {
  $res = array_merge($res, ['']);
  $i = $i + 1;
};
  if (amb($wordset, $res, 0)) {
  $out = '[' . $res[0];
  $j = 1;
  while ($j < count($res)) {
  $out = $out . ' ' . $res[$j];
  $j = $j + 1;
};
  $out = $out . ']';
  echo rtrim($out), PHP_EOL;
} else {
  echo rtrim('No amb found'), PHP_EOL;
}
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
