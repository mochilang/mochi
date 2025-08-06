<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$G = ['A' => ['B' => 2, 'C' => 5], 'B' => ['A' => 2, 'D' => 3, 'E' => 1, 'F' => 1], 'C' => ['A' => 5, 'F' => 3], 'D' => ['B' => 3], 'E' => ['B' => 4, 'F' => 3], 'F' => ['C' => 3, 'E' => 3]];
$heap = [['node' => 'E', 'cost' => 0]];
$visited = [];
$result = -1;
while (count($heap) > 0) {
  $best_idx = 0;
  $i = 1;
  while ($i < count($heap)) {
  if ($heap[$i]['cost'] < $heap[$best_idx]['cost']) {
  $best_idx = $i;
}
  $i = $i + 1;
};
  $best = $heap[$best_idx];
  $new_heap = [];
  $j = 0;
  while ($j < count($heap)) {
  if ($j != $best_idx) {
  $new_heap = _append($new_heap, $heap[$j]);
}
  $j = $j + 1;
};
  $heap = $new_heap;
  $u = $best['node'];
  $cost = $best['cost'];
  if (array_key_exists($u, $visited)) {
  continue;
}
  $visited[$u] = true;
  if ($u == 'C') {
  $result = $cost;
  break;
}
  foreach (array_keys($G[$u]) as $v) {
  if (array_key_exists($v, $visited)) {
  continue;
}
  $next_cost = $cost + $G[$u][$v];
  $heap = _append($heap, ['node' => $v, $cost => $next_cost]);
};
}
echo rtrim(json_encode($result, 1344)), PHP_EOL;
$G2 = ['B' => ['C' => 1], 'C' => ['D' => 1], 'D' => ['F' => 1], 'E' => ['B' => 1, 'F' => 3], 'F' => []];
$heap2 = [['node' => 'E', 'cost' => 0]];
$visited2 = [];
$result2 = -1;
while (count($heap2) > 0) {
  $best2_idx = 0;
  $i2 = 1;
  while ($i2 < count($heap2)) {
  if ($heap2[$i2]['cost'] < $heap2[$best2_idx]['cost']) {
  $best2_idx = $i2;
}
  $i2 = $i2 + 1;
};
  $best2 = $heap2[$best2_idx];
  $new_heap2 = [];
  $j2 = 0;
  while ($j2 < count($heap2)) {
  if ($j2 != $best2_idx) {
  $new_heap2 = _append($new_heap2, $heap2[$j2]);
}
  $j2 = $j2 + 1;
};
  $heap2 = $new_heap2;
  $u2 = $best2['node'];
  $cost2 = $best2['cost'];
  if (array_key_exists($u2, $visited2)) {
  continue;
}
  $visited2[$u2] = true;
  if ($u2 == 'F') {
  $result2 = $cost2;
  break;
}
  foreach (array_keys($G2[$u2]) as $v2) {
  if (array_key_exists($v2, $visited2)) {
  continue;
}
  $next_cost2 = $cost2 + $G2[$u2][$v2];
  $heap2 = _append($heap2, ['node' => $v2, $cost => $next_cost2]);
};
}
echo rtrim(json_encode($result2, 1344)), PHP_EOL;
$G3 = ['B' => ['C' => 1], 'C' => ['D' => 1], 'D' => ['F' => 1], 'E' => ['B' => 1, 'G' => 2], 'F' => [], 'G' => ['F' => 1]];
$heap3 = [['node' => 'E', 'cost' => 0]];
$visited3 = [];
$result3 = -1;
while (count($heap3) > 0) {
  $best3_idx = 0;
  $i3 = 1;
  while ($i3 < count($heap3)) {
  if ($heap3[$i3]['cost'] < $heap3[$best3_idx]['cost']) {
  $best3_idx = $i3;
}
  $i3 = $i3 + 1;
};
  $best3 = $heap3[$best3_idx];
  $new_heap3 = [];
  $j3 = 0;
  while ($j3 < count($heap3)) {
  if ($j3 != $best3_idx) {
  $new_heap3 = _append($new_heap3, $heap3[$j3]);
}
  $j3 = $j3 + 1;
};
  $heap3 = $new_heap3;
  $u3 = $best3['node'];
  $cost3 = $best3['cost'];
  if (array_key_exists($u3, $visited3)) {
  continue;
}
  $visited3[$u3] = true;
  if ($u3 == 'F') {
  $result3 = $cost3;
  break;
}
  foreach (array_keys($G3[$u3]) as $v3) {
  if (array_key_exists($v3, $visited3)) {
  continue;
}
  $next_cost3 = $cost3 + $G3[$u3][$v3];
  $heap3 = _append($heap3, ['node' => $v3, $cost => $next_cost3]);
};
}
echo rtrim(json_encode($result3, 1344)), PHP_EOL;
