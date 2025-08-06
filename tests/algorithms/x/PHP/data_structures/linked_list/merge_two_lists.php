<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
function sort_list($nums) {
  $arr = [];
  $i = 0;
  while ($i < count($nums)) {
  $arr = _append($arr, $nums[$i]);
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($arr)) {
  $k = $j + 1;
  while ($k < count($arr)) {
  if ($arr[$k] < $arr[$j]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$k];
  $arr[$k] = $tmp;
}
  $k = $k + 1;
};
  $j = $j + 1;
};
  return $arr;
}
function make_sorted_linked_list($ints) {
  return ['values' => sort_list($ints)];
}
function len_sll($sll) {
  return _len($sll['values']);
}
function str_sll($sll) {
  $res = '';
  $i = 0;
  while ($i < _len($sll['values'])) {
  $res = $res . _str($sll['values'][$i]);
  if ($i + 1 < _len($sll['values'])) {
  $res = $res . ' -> ';
}
  $i = $i + 1;
};
  return $res;
}
function merge_lists($a, $b) {
  $combined = [];
  $i = 0;
  while ($i < _len($a['values'])) {
  $combined = _append($combined, $a[$values][$i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < _len($b['values'])) {
  $combined = _append($combined, $b[$values][$i]);
  $i = $i + 1;
};
  return make_sorted_linked_list($combined);
}
function main() {
  $test_data_odd = [3, 9, -11, 0, 7, 5, 1, -1];
  $test_data_even = [4, 6, 2, 0, 8, 10, 3, -2];
  $sll_one = make_sorted_linked_list($test_data_odd);
  $sll_two = make_sorted_linked_list($test_data_even);
  $merged = merge_lists($sll_one, $sll_two);
  echo rtrim(_str(len_sll($merged))), PHP_EOL;
  echo rtrim(str_sll($merged)), PHP_EOL;
}
main();
