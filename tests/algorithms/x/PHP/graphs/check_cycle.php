<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function depth_first_search($graph, $vertex, &$visited, &$rec_stk) {
  global $g1, $g2;
  $visited[$vertex] = true;
  $rec_stk[$vertex] = true;
  foreach ($graph[$vertex] as $node) {
  if (!$visited[$node]) {
  if (depth_first_search($graph, $node, $visited, $rec_stk)) {
  return true;
};
} else {
  if ($rec_stk[$node]) {
  return true;
};
}
};
  $rec_stk[$vertex] = false;
  return false;
}
function check_cycle($graph) {
  global $g1, $g2;
  $n = count($graph);
  $visited = [];
  $rec_stk = [];
  $i = 0;
  while ($i < $n) {
  $visited = _append($visited, false);
  $rec_stk = _append($rec_stk, false);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  if (!$visited[$i]) {
  if (depth_first_search($graph, $i, $visited, $rec_stk)) {
  return true;
};
}
  $i = $i + 1;
};
  return false;
}
function print_bool($b) {
  global $g1, $g2;
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
}
$g1 = [[], [0, 3], [0, 4], [5], [5], []];
print_bool(check_cycle($g1));
$g2 = [[1, 2], [2], [0, 3], [3]];
print_bool(check_cycle($g2));
