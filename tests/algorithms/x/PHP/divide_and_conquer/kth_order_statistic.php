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
function pivot($lst) {
  return $lst[0];
}
function kth_number($lst, $k) {
  $p = pivot($lst);
  $small = [];
  $big = [];
  $i = 0;
  while ($i < count($lst)) {
  $e = $lst[$i];
  if ($e < $p) {
  $small = _append($small, $e);
} else {
  if ($e > $p) {
  $big = _append($big, $e);
};
}
  $i = $i + 1;
};
  if (count($small) == $k - 1) {
  return $p;
} else {
  if (count($small) < $k - 1) {
  return kth_number($big, $k - count($small) - 1);
} else {
  return kth_number($small, $k);
};
}
}
echo rtrim(_str(kth_number([2, 1, 3, 4, 5], 3))), PHP_EOL;
echo rtrim(_str(kth_number([2, 1, 3, 4, 5], 1))), PHP_EOL;
echo rtrim(_str(kth_number([2, 1, 3, 4, 5], 5))), PHP_EOL;
echo rtrim(_str(kth_number([3, 2, 5, 6, 7, 8], 2))), PHP_EOL;
echo rtrim(_str(kth_number([25, 21, 98, 100, 76, 22, 43, 60, 89, 87], 4))), PHP_EOL;
