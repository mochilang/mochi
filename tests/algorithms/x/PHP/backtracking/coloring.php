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
  function valid_coloring($neighbours, $colored_vertices, $color) {
  global $graph;
  $i = 0;
  while ($i < count($neighbours)) {
  if ($neighbours[$i] == 1 && $colored_vertices[$i] == 'color') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function util_color($graph, $max_colors, &$colored_vertices, $index) {
  if ($index == count($graph)) {
  return true;
}
  $c = 0;
  while ($c < $max_colors) {
  if (valid_coloring($graph[$index], $colored_vertices, $c)) {
  $colored_vertices[$index] = $c;
  if (util_color($graph, $max_colors, $colored_vertices, $index + 1)) {
  return true;
};
  $colored_vertices[$index] = -1;
}
  $c = $c + 1;
};
  return false;
};
  function color($graph, $max_colors) {
  $colored_vertices = [];
  $i = 0;
  while ($i < count($graph)) {
  $colored_vertices = _append($colored_vertices, -1);
  $i = $i + 1;
};
  if (util_color($graph, $max_colors, $colored_vertices, 0)) {
  return $colored_vertices;
}
  return [];
};
  $graph = [[0, 1, 0, 0, 0], [1, 0, 1, 0, 1], [0, 1, 0, 1, 0], [0, 1, 1, 0, 0], [0, 1, 0, 0, 0]];
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(color($graph, 3), 1344))))))), PHP_EOL;
  echo rtrim('
'), PHP_EOL;
  echo rtrim(json_encode(count(color($graph, 2)), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
