<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function bubble_sort($nums) {
  $arr = $nums;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $temp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $temp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function three_sum($nums) {
  $sorted = bubble_sort($nums);
  $res = [];
  $n = count($sorted);
  $i = 0;
  while ($i < $n - 2) {
  if ($i == 0 || $sorted[$i] != $sorted[$i - 1]) {
  $low = $i + 1;
  $high = $n - 1;
  $c = 0 - $sorted[$i];
  while ($low < $high) {
  $s = $sorted[$low] + $sorted[$high];
  if ($s == $c) {
  $triple = [$sorted[$i], $sorted[$low], $sorted[$high]];
  $res = _append($res, $triple);
  while ($low < $high && $sorted[$low] == $sorted[$low + 1]) {
  $low = $low + 1;
};
  while ($low < $high && $sorted[$high] == $sorted[$high - 1]) {
  $high = $high - 1;
};
  $low = $low + 1;
  $high = $high - 1;
} else {
  if ($s < $c) {
  $low = $low + 1;
} else {
  $high = $high - 1;
};
}
};
}
  $i = $i + 1;
};
  return $res;
}
echo rtrim(_str(three_sum([-1, 0, 1, 2, -1, -4]))), PHP_EOL;
echo rtrim(_str(three_sum([1, 2, 3, 4]))), PHP_EOL;
