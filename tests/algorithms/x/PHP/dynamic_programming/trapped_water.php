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
function make_list($len, $value) {
  $arr = [];
  $i = 0;
  while ($i < $len) {
  $arr = _append($arr, $value);
  $i = $i + 1;
};
  return $arr;
}
function trapped_rainwater($heights) {
  if (count($heights) == 0) {
  return 0;
}
  $i = 0;
  while ($i < count($heights)) {
  if ($heights[$i] < 0) {
  $panic('No height can be negative');
}
  $i = $i + 1;
};
  $length = count($heights);
  $left_max = make_list($length, 0);
  $left_max[0] = $heights[0];
  $i = 1;
  while ($i < $length) {
  if ($heights[$i] > $left_max[$i - 1]) {
  $left_max[$i] = $heights[$i];
} else {
  $left_max[$i] = $left_max[$i - 1];
}
  $i = $i + 1;
};
  $right_max = make_list($length, 0);
  $last = $length - 1;
  $right_max[$last] = $heights[$last];
  $i = $last - 1;
  while ($i >= 0) {
  if ($heights[$i] > $right_max[$i + 1]) {
  $right_max[$i] = $heights[$i];
} else {
  $right_max[$i] = $right_max[$i + 1];
}
  $i = $i - 1;
};
  $total = 0;
  $i = 0;
  while ($i < $length) {
  $left = $left_max[$i];
  $right = $right_max[$i];
  $smaller = ($left < $right ? $left : $right);
  $total = $total + ($smaller - $heights[$i]);
  $i = $i + 1;
};
  return $total;
}
echo rtrim(_str(trapped_rainwater([0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]))), PHP_EOL;
echo rtrim(_str(trapped_rainwater([7, 1, 5, 3, 6, 4]))), PHP_EOL;
