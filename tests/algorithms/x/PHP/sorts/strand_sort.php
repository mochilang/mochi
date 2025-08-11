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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function merge($xs, $ys, $reverse) {
  $result = [];
  $i = 0;
  $j = 0;
  while ($i < count($xs) && $j < count($ys)) {
  if ($reverse) {
  if ($xs[$i] > $ys[$j]) {
  $result = _append($result, $xs[$i]);
  $i = _iadd($i, 1);
} else {
  $result = _append($result, $ys[$j]);
  $j = _iadd($j, 1);
};
} else {
  if ($xs[$i] < $ys[$j]) {
  $result = _append($result, $xs[$i]);
  $i = _iadd($i, 1);
} else {
  $result = _append($result, $ys[$j]);
  $j = _iadd($j, 1);
};
}
};
  while ($i < count($xs)) {
  $result = _append($result, $xs[$i]);
  $i = _iadd($i, 1);
};
  while ($j < count($ys)) {
  $result = _append($result, $ys[$j]);
  $j = _iadd($j, 1);
};
  return $result;
}
function strand_sort_rec($arr, $reverse, $solution) {
  if (count($arr) == 0) {
  return $solution;
}
  $sublist = [];
  $remaining = [];
  $sublist = _append($sublist, $arr[0]);
  $last = $arr[0];
  $k = 1;
  while ($k < count($arr)) {
  $item = $arr[$k];
  if ($reverse) {
  if ($item < $last) {
  $sublist = _append($sublist, $item);
  $last = $item;
} else {
  $remaining = _append($remaining, $item);
};
} else {
  if ($item > $last) {
  $sublist = _append($sublist, $item);
  $last = $item;
} else {
  $remaining = _append($remaining, $item);
};
}
  $k = _iadd($k, 1);
};
  $solution = merge($solution, $sublist, $reverse);
  return strand_sort_rec($remaining, $reverse, $solution);
}
function strand_sort($arr, $reverse) {
  return strand_sort_rec($arr, $reverse, []);
}
echo rtrim(_str(strand_sort([4, 3, 5, 1, 2], false))), PHP_EOL;
echo rtrim(_str(strand_sort([4, 3, 5, 1, 2], true))), PHP_EOL;
