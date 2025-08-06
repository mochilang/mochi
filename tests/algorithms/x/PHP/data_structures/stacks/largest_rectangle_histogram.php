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
function largest_rectangle_area($heights) {
  $stack = [];
  $max_area = 0;
  $hs = $heights;
  $hs = _append($hs, 0);
  $i = 0;
  while ($i < count($hs)) {
  while (count($stack) > 0 && $hs[$i] < $hs[$stack[count($stack) - 1]]) {
  $top = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $height = $hs[$top];
  $width = $i;
  if (count($stack) > 0) {
  $width = $i - $stack[count($stack) - 1] - 1;
}
  $area = $height * $width;
  if ($area > $max_area) {
  $max_area = $area;
}
};
  $stack = _append($stack, $i);
  $i = $i + 1;
};
  return $max_area;
}
echo rtrim(_str(largest_rectangle_area([2, 1, 5, 6, 2, 3]))), PHP_EOL;
echo rtrim(_str(largest_rectangle_area([2, 4]))), PHP_EOL;
echo rtrim(_str(largest_rectangle_area([6, 2, 5, 4, 5, 1, 6]))), PHP_EOL;
echo rtrim(_str(largest_rectangle_area([1]))), PHP_EOL;
