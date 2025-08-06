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
  function create_state_space_tree($sequence, $current, $index) {
  global $seq, $seq2;
  if ($index == count($sequence)) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($current, 1344))))))), PHP_EOL;
  return;
}
  create_state_space_tree($sequence, $current, $index + 1);
  $with_elem = _append($current, $sequence[$index]);
  create_state_space_tree($sequence, $with_elem, $index + 1);
};
  function generate_all_subsequences($sequence) {
  global $seq, $seq2;
  create_state_space_tree($sequence, [], 0);
};
  $seq = [1, 2, 3];
  generate_all_subsequences($seq);
  $seq2 = ['A', 'B', 'C'];
  generate_all_subsequences($seq2);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
