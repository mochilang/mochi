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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function contains($xs, $value) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function jaccard_similarity($set_a, $set_b, $alternative_union) {
  $intersection_len = 0;
  $i = 0;
  while ($i < count($set_a)) {
  if (contains($set_b, $set_a[$i])) {
  $intersection_len = $intersection_len + 1;
}
  $i = $i + 1;
};
  $union_len = 0;
  if ($alternative_union) {
  $union_len = count($set_a) + count($set_b);
} else {
  $union_list = [];
  $i = 0;
  while ($i < count($set_a)) {
  $val_a = $set_a[$i];
  if (!contains($union_list, $val_a)) {
  $union_list = _append($union_list, $val_a);
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($set_b)) {
  $val_b = $set_b[$i];
  if (!contains($union_list, $val_b)) {
  $union_list = _append($union_list, $val_b);
}
  $i = $i + 1;
};
  $union_len = count($union_list);
}
  return 1.0 * $intersection_len / $union_len;
};
  function main() {
  $set_a = ['a', 'b', 'c', 'd', 'e'];
  $set_b = ['c', 'd', 'e', 'f', 'h', 'i'];
  echo rtrim(json_encode(jaccard_similarity($set_a, $set_b, false), 1344)), PHP_EOL;
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
