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
  function valid_connection($graph, $next_ver, $curr_ind, $path) {
  if ($graph[$path[$curr_ind - 1]][$next_ver] == 0) {
  return false;
}
  foreach ($path as $v) {
  if ($v == $next_ver) {
  return false;
}
};
  return true;
};
  function util_hamilton_cycle($graph, &$path, $curr_ind) {
  if ($curr_ind == count($graph)) {
  return $graph[$path[$curr_ind - 1]][$path[0]] == 1;
}
  $next_ver = 0;
  while ($next_ver < count($graph)) {
  if (valid_connection($graph, $next_ver, $curr_ind, $path)) {
  $path[$curr_ind] = $next_ver;
  if (util_hamilton_cycle($graph, $path, $curr_ind + 1)) {
  return true;
};
  $path[$curr_ind] = -1;
}
  $next_ver = $next_ver + 1;
};
  return false;
};
  function hamilton_cycle($graph, $start_index) {
  $path = null;
  $i = 0;
  while ($i < count($graph) + 1) {
  $path[$i] = -1;
  $i = $i + 1;
};
  $path[0] = $start_index;
  $last = count($path) - 1;
  $path[$last] = $start_index;
  if (util_hamilton_cycle($graph, $path, 1)) {
  return $path;
}
  return [];
};
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
