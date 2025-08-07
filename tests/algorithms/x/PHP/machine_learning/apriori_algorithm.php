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
  function load_data() {
  global $frequent_itemsets;
  return [['milk'], ['milk', 'butter'], ['milk', 'bread'], ['milk', 'bread', 'chips']];
};
  function contains_string($xs, $s) {
  global $frequent_itemsets;
  foreach ($xs as $v) {
  if ($v == $s) {
  return true;
}
};
  return false;
};
  function is_subset($candidate, $transaction) {
  global $frequent_itemsets;
  foreach ($candidate as $it) {
  if (!contains_string($transaction, $it)) {
  return false;
}
};
  return true;
};
  function lists_equal($a, $b) {
  global $frequent_itemsets;
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function contains_list($itemset, $item) {
  global $frequent_itemsets;
  foreach ($itemset as $l) {
  if (lists_equal($l, $item)) {
  return true;
}
};
  return false;
};
  function count_list($itemset, $item) {
  global $frequent_itemsets;
  $c = 0;
  foreach ($itemset as $l) {
  if (lists_equal($l, $item)) {
  $c = $c + 1;
}
};
  return $c;
};
  function slice_list($xs, $start) {
  global $frequent_itemsets;
  $res = [];
  $i = $start;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function combinations_lists($xs, $k) {
  global $frequent_itemsets;
  $result = [];
  if ($k == 0) {
  $result = _append($result, []);
  return $result;
}
  $i = 0;
  while ($i < count($xs)) {
  $head = $xs[$i];
  $tail = slice_list($xs, $i + 1);
  $tail_combos = combinations_lists($tail, $k - 1);
  foreach ($tail_combos as $combo) {
  $new_combo = [];
  $new_combo = _append($new_combo, $head);
  foreach ($combo as $c) {
  $new_combo = _append($new_combo, $c);
};
  $result = _append($result, $new_combo);
};
  $i = $i + 1;
};
  return $result;
};
  function prune($itemset, $candidates, $length) {
  global $frequent_itemsets;
  $pruned = [];
  foreach ($candidates as $candidate) {
  $is_subsequence = true;
  foreach ($candidate as $item) {
  if (!contains_list($itemset, $item) || count_list($itemset, $item) < $length - 1) {
  $is_subsequence = false;
  break;
}
};
  if ($is_subsequence) {
  $pruned = _append($pruned, $candidate);
}
};
  return $pruned;
};
  function sort_strings($xs) {
  global $frequent_itemsets;
  $res = [];
  foreach ($xs as $s) {
  $res = _append($res, $s);
};
  $i = 0;
  while ($i < count($res)) {
  $j = $i + 1;
  while ($j < count($res)) {
  if ($res[$j] < $res[$i]) {
  $tmp = $res[$i];
  $res[$i] = $res[$j];
  $res[$j] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $res;
};
  function itemset_to_string($xs) {
  global $frequent_itemsets;
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $s = $s . ', ';
}
  $s = $s . '\'' . $xs[$i] . '\'';
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function apriori($data, $min_support) {
  global $frequent_itemsets;
  $itemset = [];
  foreach ($data as $transaction) {
  $t = [];
  foreach ($transaction as $v) {
  $t = _append($t, $v);
};
  $itemset = _append($itemset, $t);
};
  $frequent = [];
  $length = 1;
  while (count($itemset) > 0) {
  $counts = [];
  $idx = 0;
  while ($idx < count($itemset)) {
  $counts = _append($counts, 0);
  $idx = $idx + 1;
};
  foreach ($data as $transaction) {
  $j = 0;
  while ($j < count($itemset)) {
  $candidate = $itemset[$j];
  if (is_subset($candidate, $transaction)) {
  $counts[$j] = $counts[$j] + 1;
}
  $j = $j + 1;
};
};
  $new_itemset = [];
  $k = 0;
  while ($k < count($itemset)) {
  if ($counts[$k] >= $min_support) {
  $new_itemset = _append($new_itemset, $itemset[$k]);
}
  $k = $k + 1;
};
  $itemset = $new_itemset;
  $m = 0;
  while ($m < count($itemset)) {
  $sorted_item = sort_strings($itemset[$m]);
  $frequent = _append($frequent, ['items' => $sorted_item, 'support' => $counts[$m]]);
  $m = $m + 1;
};
  $length = $length + 1;
  $combos = combinations_lists($itemset, $length);
  $itemset = prune($itemset, $combos, $length);
};
  return $frequent;
};
  $frequent_itemsets = apriori(load_data(), 2);
  foreach ($frequent_itemsets as $fi) {
  echo rtrim(itemset_to_string($fi['items']) . ': ' . _str($fi['support'])), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
