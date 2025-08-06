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
function copy_list($arr) {
  $result = [];
  $i = 0;
  while ($i < count($arr)) {
  $result = _append($result, $arr[$i]);
  $i = $i + 1;
};
  return $result;
}
function heaps(&$arr) {
  if (count($arr) <= 1) {
  $single = [];
  return _append($single, copy_list($arr));
}
  $n = count($arr);
  $c = [];
  $i = 0;
  while ($i < $n) {
  $c = _append($c, 0);
  $i = $i + 1;
};
  $res = [];
  $res = _append($res, copy_list($arr));
  $i = 0;
  while ($i < $n) {
  if ($c[$i] < $i) {
  if ($i % 2 == 0) {
  $temp = $arr[0];
  $arr[0] = $arr[$i];
  $arr[$i] = $temp;
} else {
  $temp = $arr[$c[$i]];
  $arr[$c[$i]] = $arr[$i];
  $arr[$i] = $temp;
};
  $res = _append($res, copy_list($arr));
  $c[$i] = $c[$i] + 1;
  $i = 0;
} else {
  $c[$i] = 0;
  $i = $i + 1;
}
};
  return $res;
}
echo rtrim(_str(heaps([1, 2, 3]))), PHP_EOL;
