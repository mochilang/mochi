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
function binary_search_insertion_from($sorted_list, $item, $start) {
  $left = $start;
  $right = _isub(count($sorted_list), 1);
  while ($left <= $right) {
  $middle = _intdiv((_iadd($left, $right)), 2);
  if ($left == $right) {
  if ($sorted_list[$middle] < $item) {
  $left = _iadd($middle, 1);
};
  break;
} else {
  if ($sorted_list[$middle] < $item) {
  $left = _iadd($middle, 1);
} else {
  $right = _isub($middle, 1);
};
}
};
  $result = [];
  $i = 0;
  while ($i < $left) {
  $result = _append($result, $sorted_list[$i]);
  $i = _iadd($i, 1);
};
  $result = _append($result, $item);
  while ($i < count($sorted_list)) {
  $result = _append($result, $sorted_list[$i]);
  $i = _iadd($i, 1);
};
  return $result;
}
function binary_search_insertion($sorted_list, $item) {
  return binary_search_insertion_from($sorted_list, $item, 0);
}
function merge($left, $right) {
  $result = [];
  $i = 0;
  $j = 0;
  while ($i < count($left) && $j < count($right)) {
  if ($left[$i][0] < $right[$j][0]) {
  $result = _append($result, $left[$i]);
  $i = _iadd($i, 1);
} else {
  $result = _append($result, $right[$j]);
  $j = _iadd($j, 1);
}
};
  while ($i < count($left)) {
  $result = _append($result, $left[$i]);
  $i = _iadd($i, 1);
};
  while ($j < count($right)) {
  $result = _append($result, $right[$j]);
  $j = _iadd($j, 1);
};
  return $result;
}
function sortlist_2d($list_2d) {
  $length = count($list_2d);
  if ($length <= 1) {
  return $list_2d;
}
  $middle = _intdiv($length, 2);
  $left = [];
  $i = 0;
  while ($i < $middle) {
  $left = _append($left, $list_2d[$i]);
  $i = _iadd($i, 1);
};
  $right = [];
  $j = $middle;
  while ($j < $length) {
  $right = _append($right, $list_2d[$j]);
  $j = _iadd($j, 1);
};
  return merge(sortlist_2d($left), sortlist_2d($right));
}
function merge_insertion_sort($collection) {
  if (count($collection) <= 1) {
  return $collection;
}
  $two_paired_list = [];
  $has_last_odd_item = false;
  $i = 0;
  while ($i < count($collection)) {
  if ($i == _isub(count($collection), 1)) {
  $has_last_odd_item = true;
} else {
  $a = $collection[$i];
  $b = $collection[_iadd($i, 1)];
  if ($a < $b) {
  $two_paired_list = _append($two_paired_list, [$a, $b]);
} else {
  $two_paired_list = _append($two_paired_list, [$b, $a]);
};
}
  $i = _iadd($i, 2);
};
  $sorted_list_2d = sortlist_2d($two_paired_list);
  $result = [];
  $i = 0;
  while ($i < count($sorted_list_2d)) {
  $result = _append($result, $sorted_list_2d[$i][0]);
  $i = _iadd($i, 1);
};
  $result = _append($result, $sorted_list_2d[_isub(count($sorted_list_2d), 1)][1]);
  if ($has_last_odd_item) {
  $result = binary_search_insertion($result, $collection[_isub(count($collection), 1)]);
}
  $inserted_before = false;
  $idx = 0;
  while ($idx < _isub(count($sorted_list_2d), 1)) {
  if ($has_last_odd_item && $result[$idx] == $collection[_isub(count($collection), 1)]) {
  $inserted_before = true;
}
  $pivot = $sorted_list_2d[$idx][1];
  if ($inserted_before) {
  $result = binary_search_insertion_from($result, $pivot, _iadd($idx, 2));
} else {
  $result = binary_search_insertion_from($result, $pivot, _iadd($idx, 1));
}
  $idx = _iadd($idx, 1);
};
  return $result;
}
function main() {
  $example1 = [0, 5, 3, 2, 2];
  $example2 = [99];
  $example3 = [-2, -5, -45];
  echo rtrim(_str(merge_insertion_sort($example1))), PHP_EOL;
  echo rtrim(_str(merge_insertion_sort($example2))), PHP_EOL;
  echo rtrim(_str(merge_insertion_sort($example3))), PHP_EOL;
}
main();
