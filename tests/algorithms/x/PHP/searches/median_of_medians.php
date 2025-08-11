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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
function set_at_int($xs, $idx, $value) {
  $i = 0;
  $res = [];
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function sort_int($xs) {
  $res = $xs;
  $i = 1;
  while ($i < count($res)) {
  $key = $res[$i];
  $j = _isub($i, 1);
  while ($j >= 0 && $res[$j] > $key) {
  $res = set_at_int($res, _iadd($j, 1), $res[$j]);
  $j = _isub($j, 1);
};
  $res = set_at_int($res, _iadd($j, 1), $key);
  $i = _iadd($i, 1);
};
  return $res;
}
function median_of_five($arr) {
  $sorted = sort_int($arr);
  return $sorted[_idiv(count($sorted), 2)];
}
function median_of_medians($arr) {
  if (count($arr) <= 5) {
  return median_of_five($arr);
}
  $medians = [];
  $i = 0;
  while ($i < count($arr)) {
  if (_iadd($i, 5) <= count($arr)) {
  $medians = _append($medians, median_of_five(array_slice($arr, $i, _iadd($i, 5) - $i)));
} else {
  $medians = _append($medians, median_of_five(array_slice($arr, $i, count($arr) - $i)));
}
  $i = _iadd($i, 5);
};
  return median_of_medians($medians);
}
function quick_select($arr, $target) {
  if ($target > count($arr)) {
  return -1;
}
  $x = median_of_medians($arr);
  $left = [];
  $right = [];
  $check = false;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] < $x) {
  $left = _append($left, $arr[$i]);
} else {
  if ($arr[$i] > $x) {
  $right = _append($right, $arr[$i]);
} else {
  if ($arr[$i] == $x) {
  if (!$check) {
  $check = true;
} else {
  $right = _append($right, $arr[$i]);
};
} else {
  $right = _append($right, $arr[$i]);
};
};
}
  $i = _iadd($i, 1);
};
  $rank_x = _iadd(count($left), 1);
  $answer = 0;
  if ($rank_x == $target) {
  $answer = $x;
} else {
  if ($rank_x > $target) {
  $answer = quick_select($left, $target);
} else {
  $answer = quick_select($right, _isub($target, $rank_x));
};
}
  return $answer;
}
function main() {
  echo rtrim(_str(median_of_five([5, 4, 3, 2]))), PHP_EOL;
  echo rtrim(_str(quick_select([2, 4, 5, 7, 899, 54, 32], 5))), PHP_EOL;
}
main();
