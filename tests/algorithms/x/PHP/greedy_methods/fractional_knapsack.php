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
function sort_by_ratio_desc(&$arr) {
  global $vl, $wt, $result;
  $i = 1;
  while ($i < count($arr)) {
  $key = $arr[$i];
  $j = $i - 1;
  while ($j >= 0) {
  $current = $arr[$j];
  if ($current['value'] / $current['weight'] < $key['value'] / $key['weight']) {
  $arr[$j + 1] = $current;
  $j = $j - 1;
} else {
  break;
}
};
  $arr[$j + 1] = $key;
  $i = $i + 1;
};
  return $arr;
}
function sum_first($arr, $k) {
  global $vl, $wt, $result;
  $s = 0.0;
  $i = 0;
  while ($i < $k && $i < count($arr)) {
  $s = $s + $arr[$i];
  $i = $i + 1;
};
  return $s;
}
function frac_knapsack($vl, $wt, $w, $n) {
  global $result;
  $items = [];
  $i = 0;
  while ($i < count($vl) && $i < count($wt)) {
  $items = _append($items, ['value' => $vl[$i], 'weight' => $wt[$i]]);
  $i = $i + 1;
};
  $items = sort_by_ratio_desc($items);
  $values = [];
  $weights = [];
  $i = 0;
  while ($i < count($items)) {
  $itm = $items[$i];
  $values = _append($values, $itm['value']);
  $weights = _append($weights, $itm['weight']);
  $i = $i + 1;
};
  $acc = [];
  $total = 0.0;
  $i = 0;
  while ($i < count($weights)) {
  $total = $total + $weights[$i];
  $acc = _append($acc, $total);
  $i = $i + 1;
};
  $k = 0;
  while ($k < count($acc) && $w >= $acc[$k]) {
  $k = $k + 1;
};
  if ($k == 0) {
  return 0.0;
}
  if ($k >= count($values)) {
  return sum_first($values, count($values));
}
  if ($k != $n) {
  return sum_first($values, $k) + ($w - $acc[$k - 1]) * $values[$k] / $weights[$k];
}
  return sum_first($values, $k);
}
$vl = [60.0, 100.0, 120.0];
$wt = [10.0, 20.0, 30.0];
$result = frac_knapsack($vl, $wt, 50.0, 3);
echo rtrim(_str($result)), PHP_EOL;
