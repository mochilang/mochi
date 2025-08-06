<?php
ini_set('memory_limit', '-1');
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
function prims_algorithm($adjacency) {
  $visited = [];
  $visited[0] = true;
  $mst = [];
  $count = 1;
  $total = 0;
  foreach (array_keys($adjacency) as $k) {
  $total = $total + 1;
};
  while ($count < $total) {
  $best_u = 0;
  $best_v = 0;
  $best_cost = 2147483647;
  foreach (array_keys($adjacency) as $u_str) {
  $u = intval($u_str);
  if ($visited[$u]) {
  foreach ($adjacency[$u] as $n) {
  if (!$visited[$n['node']] && $n['cost'] < $best_cost) {
  $best_cost = $n['cost'];
  $best_u = $u;
  $best_v = $n['node'];
}
};
}
};
  $visited[$best_v] = true;
  $mst = _append($mst, [$u => $best_u, 'v' => $best_v]);
  $count = $count + 1;
};
  return $mst;
}
function test_prim_successful_result() {
  $edges = [[0, 1, 4], [0, 7, 8], [1, 2, 8], [7, 8, 7], [7, 6, 1], [2, 8, 2], [8, 6, 6], [2, 3, 7], [2, 5, 4], [6, 5, 2], [3, 5, 14], [3, 4, 9], [5, 4, 10], [1, 7, 11]];
  $adjacency = [];
  foreach ($edges as $e) {
  $u = $e[0];
  $v = $e[1];
  $w = $e[2];
  if (!(array_key_exists($u, $adjacency))) {
  $adjacency[$u] = [];
}
  if (!(array_key_exists($v, $adjacency))) {
  $adjacency[$v] = [];
}
  $adjacency[$u] = _append($adjacency[$u], ['node' => $v, 'cost' => $w]);
  $adjacency[$v] = _append($adjacency[$v], ['node' => $u, 'cost' => $w]);
};
  $result = prims_algorithm($adjacency);
  $seen = [];
  foreach ($result as $e) {
  $key1 = _str($e['u']) . ',' . _str($e['v']);
  $key2 = _str($e['v']) . ',' . _str($e['u']);
  $seen[$key1] = true;
  $seen[$key2] = true;
};
  $expected = [[7, 6, 1], [2, 8, 2], [6, 5, 2], [0, 1, 4], [2, 5, 4], [2, 3, 7], [0, 7, 8], [3, 4, 9]];
  foreach ($expected as $ans) {
  $key = _str($ans[0]) . ',' . _str($ans[1]);
  if (!$seen[$key]) {
  return false;
}
};
  return true;
}
echo rtrim(json_encode(test_prim_successful_result(), 1344)), PHP_EOL;
echo rtrim((true ? 'true' : 'false')), PHP_EOL;
