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
function make_set($ds, $x) {
  global $i, $j, $res_i, $root_i, $res_j, $root_j, $same, $root_same, $res;
  $p = $ds['parent'];
  $r = $ds['rank'];
  $p[$x] = $x;
  $r[$x] = 0;
  return ['parent' => $p, 'rank' => $r];
}
function find_set($ds, $x) {
  global $i, $j, $res_i, $root_i, $res_j, $root_j, $same, $root_same;
  if ($ds['parent'][$x] == $x) {
  return ['ds' => $ds, 'root' => $x];
}
  $res = find_set($ds, $ds['parent'][$x]);
  $p = $res['ds']['parent'];
  $p[$x] = $res['root'];
  return ['ds' => ['parent' => $p, 'rank' => $res['ds']['rank']], 'root' => $res['root']];
}
function union_set($ds, $x, $y) {
  global $i, $j, $res_i, $root_i, $res_j, $root_j, $same, $root_same, $res;
  $fx = find_set($ds, $x);
  $ds1 = $fx['ds'];
  $x_root = $fx['root'];
  $fy = find_set($ds1, $y);
  $ds2 = $fy['ds'];
  $y_root = $fy['root'];
  if ($x_root == $y_root) {
  return $ds2;
}
  $p = $ds2['parent'];
  $r = $ds2['rank'];
  if ($r[$x_root] > $r[$y_root]) {
  $p[$y_root] = $x_root;
} else {
  $p[$x_root] = $y_root;
  if ($r[$x_root] == $r[$y_root]) {
  $r[$y_root] = $r[$y_root] + 1;
};
}
  return ['parent' => $p, 'rank' => $r];
}
function same_python_set($a, $b) {
  global $ds, $i, $j, $res_i, $root_i, $res_j, $root_j, $same, $root_same, $res;
  if ($a < 3 && $b < 3) {
  return true;
}
  if ($a >= 3 && $a < 6 && $b >= 3 && $b < 6) {
  return true;
}
  return false;
}
$ds = ['parent' => [], 'rank' => []];
$i = 0;
while ($i < 6) {
  $ds['parent'] = _append($ds['parent'], 0);
  $ds['rank'] = _append($ds['rank'], 0);
  $ds = make_set($ds, $i);
  $i = $i + 1;
}
$ds = union_set($ds, 0, 1);
$ds = union_set($ds, 1, 2);
$ds = union_set($ds, 3, 4);
$ds = union_set($ds, 3, 5);
$i = 0;
while ($i < 6) {
  $j = 0;
  while ($j < 6) {
  $res_i = find_set($ds, $i);
  $ds = $res_i['ds'];
  $root_i = $res_i['root'];
  $res_j = find_set($ds, $j);
  $ds = $res_j['ds'];
  $root_j = $res_j['root'];
  $same = same_python_set($i, $j);
  $root_same = $root_i == $root_j;
  if ($same) {
  if (!$root_same) {
  $panic('nodes should be in same set');
};
} else {
  if ($root_same) {
  $panic('nodes should be in different sets');
};
}
  $j = $j + 1;
};
  $i = $i + 1;
}
$i = 0;
while ($i < 6) {
  $res = find_set($ds, $i);
  $ds = $res['ds'];
  echo rtrim(_str($res['root'])), PHP_EOL;
  $i = $i + 1;
}
