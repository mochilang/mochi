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
  function index_of($s, $sub, $start) {
  global $i, $sample_html, $stats;
  $n = strlen($s);
  $m = strlen($sub);
  $i = $start;
  while ($i <= $n - $m) {
  $j = 0;
  while ($j < $m && substr($s, $i + $j, $i + $j + 1 - ($i + $j)) == substr($sub, $j, $j + 1 - $j)) {
  $j = $j + 1;
};
  if ($j == $m) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function find_all($html, $open, $close) {
  global $i, $sample_html, $stats;
  $res = [];
  $pos = 0;
  $ol = strlen($open);
  $cl = strlen($close);
  while (true) {
  $start = index_of($html, $open, $pos);
  if ($start < 0) {
  break;
}
  $begin = $start + $ol;
  $end = index_of($html, $close, $begin);
  if ($end < 0) {
  break;
}
  $res = _append($res, $html[$begin]);
  $pos = $end + $cl;
};
  return $res;
};
  function world_covid19_stats($html) {
  global $i, $sample_html, $stats;
  $keys = find_all($html, '<h1>', '</h1>');
  $values = find_all($html, '<div class="maincounter-number"><span>', '</span></div>');
  $extra_keys = find_all($html, '<span class="panel-title">', '</span>');
  foreach ($extra_keys as $k) {
  $keys = _append($keys, $k);
};
  $extra_vals = find_all($html, '<div class="number-table-main">', '</div>');
  foreach ($extra_vals as $v) {
  $values = _append($values, $v);
};
  $res = [];
  $i = 0;
  while ($i < count($keys) && $i < count($values)) {
  $res = _append($res, [$keys[$i], $values[$i]]);
  $i = $i + 1;
};
  return $res;
};
  $sample_html = '<h1>Coronavirus Cases:</h1><div class="maincounter-number"><span>100</span></div><h1>Deaths:</h1><div class="maincounter-number"><span>10</span></div><h1>Recovered:</h1><div class="maincounter-number"><span>50</span></div><span class="panel-title">Active Cases</span><div class="number-table-main">20</div><span class="panel-title">Closed Cases</span><div class="number-table-main">80</div>';
  $stats = world_covid19_stats($sample_html);
  echo rtrim('COVID-19 Status of the World
'), PHP_EOL;
  $i = 0;
  while ($i < count($stats)) {
  echo rtrim($stats[$i][0]), PHP_EOL;
  echo rtrim($stats[$i][1]), PHP_EOL;
  $i = $i + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
