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
$f = null;
function max_int($a, $b) {
  global $dp_table, $example, $f, $n, $optimal_solution, $val_list, $w_cap, $wt_list;
  if ($a > $b) {
  return $a;
} else {
  return $b;
}
}
function init_f($n, $w) {
  global $dp_table, $example, $f, $optimal_solution, $val_list, $w_cap, $wt_list;
  $table = null;
  $i = 0;
  while ($i <= $n) {
  $row = null;
  $j = 0;
  while ($j <= $w) {
  if ($i == 0 || $j == 0) {
  $row = _append($row, 0);
} else {
  $row = _append($row, -1);
}
  $j = $j + 1;
};
  $table = _append($table, $row);
  $i = $i + 1;
};
  return $table;
}
function mf_knapsack($i, $wt, $val, $j) {
  global $dp_table, $example, $f, $n, $optimal_solution, $val_list, $w_cap, $wt_list;
  if ($f[$i][$j] < 0) {
  if ($j < $wt[$i - 1]) {
  $f[$i][$j] = mf_knapsack($i - 1, $wt, $val, $j);
} else {
  $without_item = mf_knapsack($i - 1, $wt, $val, $j);
  $with_item = mf_knapsack($i - 1, $wt, $val, $j - $wt[$i - 1]) + $val[$i - 1];
  $f[$i][$j] = max_int($without_item, $with_item);
};
}
  return $f[$i][$j];
}
function create_matrix($rows, $cols) {
  global $dp_table, $example, $f, $n, $optimal_solution, $val_list, $w_cap, $wt_list;
  $matrix = null;
  $i = 0;
  while ($i <= $rows) {
  $row = null;
  $j = 0;
  while ($j <= $cols) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $matrix = _append($matrix, $row);
  $i = $i + 1;
};
  return $matrix;
}
function knapsack($w, $wt, $val, $n) {
  global $dp_table, $example, $f, $optimal_solution, $val_list, $w_cap, $wt_list;
  $dp = create_matrix($n, $w);
  $i = 1;
  while ($i <= $n) {
  $w_ = 1;
  while ($w_ <= $w) {
  if ($wt[$i - 1] <= $w_) {
  $include = $val[$i - 1] + $dp[$i - 1][$w_ - $wt[$i - 1]];
  $exclude = $dp[$i - 1][$w_];
  $dp[$i][$w_] = max_int($include, $exclude);
} else {
  $dp[$i][$w_] = $dp[$i - 1][$w_];
}
  $w_ = $w_ + 1;
};
  $i = $i + 1;
};
  return $dp;
}
function construct_solution($dp, $wt, $i, $j, $optimal_set) {
  global $dp_table, $example, $f, $n, $optimal_solution, $val_list, $w_cap, $wt_list;
  if ($i > 0 && $j > 0) {
  if ($dp[$i - 1][$j] == $dp[$i][$j]) {
  return construct_solution($dp, $wt, $i - 1, $j, $optimal_set);
} else {
  $with_prev = construct_solution($dp, $wt, $i - 1, $j - $wt[$i - 1], $optimal_set);
  return _append($with_prev, $i);
};
}
  return $optimal_set;
}
function knapsack_with_example_solution($w, $wt, $val) {
  global $dp_table, $example, $f, $n, $optimal_solution, $val_list, $w_cap, $wt_list;
  $num_items = count($wt);
  $dp_table = knapsack($w, $wt, $val, $num_items);
  $optimal_val = $dp_table[$num_items][$w];
  $subset = construct_solution($dp_table, $wt, $num_items, $w, []);
  return ['value' => $optimal_val, 'subset' => $subset];
}
function format_set($xs) {
  global $dp_table, $example, $f, $n, $optimal_solution, $val_list, $w_cap, $wt_list;
  $res = '{';
  $i = 0;
  while ($i < count($xs)) {
  $res = $res . _str($xs[$i]);
  if ($i + 1 < count($xs)) {
  $res = $res . ', ';
}
  $i = $i + 1;
};
  $res = $res . '}';
  return $res;
}
$val_list = [3, 2, 4, 4];
$wt_list = [4, 3, 2, 3];
$n = 4;
$w_cap = 6;
$f = init_f($n, $w_cap);
$dp_table = knapsack($w_cap, $wt_list, $val_list, $n);
$optimal_solution = $dp_table[$n][$w_cap];
echo rtrim(json_encode($optimal_solution, 1344)), PHP_EOL;
echo rtrim(json_encode(mf_knapsack($n, $wt_list, $val_list, $w_cap), 1344)), PHP_EOL;
$example = knapsack_with_example_solution($w_cap, $wt_list, $val_list);
echo rtrim('optimal_value = ' . _str($example['value'])), PHP_EOL;
echo rtrim('An optimal subset corresponding to the optimal value ' . format_set($example['subset'])), PHP_EOL;
