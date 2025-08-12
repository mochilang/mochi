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
  $seed = 1;
  function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function random() {
  global $seed;
  return (1.0 * mochi_rand()) / 2147483648.0;
};
  function complete_graph($vertices_number) {
  global $seed;
  $graph = [];
  $i = 0;
  while ($i < $vertices_number) {
  $neighbors = [];
  $j = 0;
  while ($j < $vertices_number) {
  if ($j != $i) {
  $neighbors = _append($neighbors, $j);
}
  $j = $j + 1;
};
  $graph[$i] = $neighbors;
  $i = $i + 1;
};
  return $graph;
};
  function random_graph($vertices_number, $probability, $directed) {
  global $seed;
  $graph = [];
  $i = 0;
  while ($i < $vertices_number) {
  $graph[$i] = [];
  $i = $i + 1;
};
  if ($probability >= 1.0) {
  return complete_graph($vertices_number);
}
  if ($probability <= 0.0) {
  return $graph;
}
  $i = 0;
  while ($i < $vertices_number) {
  $j = $i + 1;
  while ($j < $vertices_number) {
  if (random() < $probability) {
  $graph[$i] = _append($graph[$i], $j);
  if (!$directed) {
  $graph[$j] = _append($graph[$j], $i);
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $graph;
};
  function main() {
  global $seed;
  $seed = 1;
  $g1 = random_graph(4, 0.5, false);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($g1, 1344)))))), PHP_EOL;
  $seed = 1;
  $g2 = random_graph(4, 0.5, true);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($g2, 1344)))))), PHP_EOL;
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
