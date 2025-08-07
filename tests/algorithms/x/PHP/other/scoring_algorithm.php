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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_data($source_data) {
  global $vehicles, $weights, $result;
  $data_lists = [];
  $i = 0;
  while ($i < count($source_data)) {
  $row = $source_data[$i];
  $j = 0;
  while ($j < count($row)) {
  if (count($data_lists) < $j + 1) {
  $empty = [];
  $data_lists = _append($data_lists, $empty);
}
  $data_lists[$j] = _append($data_lists[$j], $row[$j]);
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $data_lists;
};
  function calculate_each_score($data_lists, $weights) {
  global $vehicles, $result;
  $score_lists = [];
  $i = 0;
  while ($i < count($data_lists)) {
  $dlist = $data_lists[$i];
  $weight = $weights[$i];
  $mind = $dlist[0];
  $maxd = $dlist[0];
  $j = 1;
  while ($j < count($dlist)) {
  $val = $dlist[$j];
  if ($val < $mind) {
  $mind = $val;
}
  if ($val > $maxd) {
  $maxd = $val;
}
  $j = $j + 1;
};
  $score = [];
  $j = 0;
  if ($weight == 0) {
  while ($j < count($dlist)) {
  $item = $dlist[$j];
  if ($maxd - $mind == 0.0) {
  $score = _append($score, 1.0);
} else {
  $score = _append($score, 1.0 - (($item - $mind) / ($maxd - $mind)));
}
  $j = $j + 1;
};
} else {
  while ($j < count($dlist)) {
  $item = $dlist[$j];
  if ($maxd - $mind == 0.0) {
  $score = _append($score, 0.0);
} else {
  $score = _append($score, ($item - $mind) / ($maxd - $mind));
}
  $j = $j + 1;
};
}
  $score_lists = _append($score_lists, $score);
  $i = $i + 1;
};
  return $score_lists;
};
  function generate_final_scores($score_lists) {
  global $vehicles, $weights, $result;
  $count = count($score_lists[0]);
  $final_scores = [];
  $i = 0;
  while ($i < $count) {
  $final_scores = _append($final_scores, 0.0);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($score_lists)) {
  $slist = $score_lists[$i];
  $j = 0;
  while ($j < count($slist)) {
  $final_scores[$j] = $final_scores[$j] + $slist[$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $final_scores;
};
  function procentual_proximity(&$source_data, $weights) {
  global $vehicles, $result;
  $data_lists = get_data($source_data);
  $score_lists = calculate_each_score($data_lists, $weights);
  $final_scores = generate_final_scores($score_lists);
  $i = 0;
  while ($i < count($final_scores)) {
  $source_data[$i] = _append($source_data[$i], $final_scores[$i]);
  $i = $i + 1;
};
  return $source_data;
};
  $vehicles = [];
  $vehicles = _append($vehicles, [20.0, 60.0, 2012.0]);
  $vehicles = _append($vehicles, [23.0, 90.0, 2015.0]);
  $vehicles = _append($vehicles, [22.0, 50.0, 2011.0]);
  $weights = [0, 0, 1];
  $result = procentual_proximity($vehicles, $weights);
  echo rtrim(_str($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
