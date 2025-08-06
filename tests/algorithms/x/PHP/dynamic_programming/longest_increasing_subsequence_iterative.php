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
function copy_list($xs) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function longest_subsequence($arr) {
  $n = count($arr);
  $lis = [];
  $i = 0;
  while ($i < $n) {
  $single = [];
  $single = _append($single, $arr[$i]);
  $lis = _append($lis, $single);
  $i = $i + 1;
};
  $i = 1;
  while ($i < $n) {
  $prev = 0;
  while ($prev < $i) {
  if ($arr[$prev] <= $arr[$i] && count($lis[$prev]) + 1 > count($lis[$i])) {
  $temp = copy_list($lis[$prev]);
  $temp2 = _append($temp, $arr[$i]);
  $lis[$i] = $temp2;
}
  $prev = $prev + 1;
};
  $i = $i + 1;
};
  $result = [];
  $i = 0;
  while ($i < $n) {
  if (count($lis[$i]) > count($result)) {
  $result = $lis[$i];
}
  $i = $i + 1;
};
  return $result;
}
function main() {
  echo rtrim(_str(longest_subsequence([10, 22, 9, 33, 21, 50, 41, 60, 80]))), PHP_EOL;
  echo rtrim(_str(longest_subsequence([4, 8, 7, 5, 1, 12, 2, 3, 9]))), PHP_EOL;
  echo rtrim(_str(longest_subsequence([9, 8, 7, 6, 5, 7]))), PHP_EOL;
  echo rtrim(_str(longest_subsequence([28, 26, 12, 23, 35, 39]))), PHP_EOL;
  echo rtrim(_str(longest_subsequence([1, 1, 1]))), PHP_EOL;
  echo rtrim(_str(longest_subsequence([]))), PHP_EOL;
}
main();
