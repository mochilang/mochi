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
function range_desc($start, $end) {
  $res = [];
  $i = $start;
  while ($i >= $end) {
  $res = _append($res, $i);
  $i = _isub($i, 1);
};
  return $res;
}
function range_asc($start, $end) {
  $res = [];
  $i = $start;
  while ($i <= $end) {
  $res = _append($res, $i);
  $i = _iadd($i, 1);
};
  return $res;
}
function concat_lists($a, $b) {
  $res = $a;
  $i = 0;
  while ($i < count($b)) {
  $res = _append($res, $b[$i]);
  $i = _iadd($i, 1);
};
  return $res;
}
function swap($xs, $i, $j) {
  $res = [];
  $k = 0;
  while ($k < count($xs)) {
  if ($k == $i) {
  $res = _append($res, $xs[$j]);
} else {
  if ($k == $j) {
  $res = _append($res, $xs[$i]);
} else {
  $res = _append($res, $xs[$k]);
};
}
  $k = _iadd($k, 1);
};
  return $res;
}
function generate_gon_ring($gon_side, $perm) {
  $result = [];
  $result = _append($result, $perm[0]);
  $result = _append($result, $perm[1]);
  $result = _append($result, $perm[2]);
  $extended = _append($perm, $perm[1]);
  $magic_number = ($gon_side < 5 ? 1 : 2);
  $i = 1;
  while ($i < _iadd(_idiv(count($extended), 3), $magic_number)) {
  $result = _append($result, $extended[_iadd(_imul(2, $i), 1)]);
  $result = _append($result, $result[_isub(_imul(3, $i), 1)]);
  $result = _append($result, $extended[_iadd(_imul(2, $i), 2)]);
  $i = _iadd($i, 1);
};
  return $result;
}
function min_outer($numbers) {
  $min_val = $numbers[0];
  $i = 3;
  while ($i < count($numbers)) {
  if ($numbers[$i] < $min_val) {
  $min_val = $numbers[$i];
}
  $i = _iadd($i, 3);
};
  return $min_val;
}
function is_magic_gon($numbers) {
  if (_imod(count($numbers), 3) != 0) {
  return false;
}
  if (min_outer($numbers) != $numbers[0]) {
  return false;
}
  $total = _iadd(_iadd($numbers[0], $numbers[1]), $numbers[2]);
  $i = 3;
  while ($i < count($numbers)) {
  if (_iadd(_iadd($numbers[$i], $numbers[_iadd($i, 1)]), $numbers[_iadd($i, 2)]) != $total) {
  return false;
}
  $i = _iadd($i, 3);
};
  return true;
}
function permute_search($nums, $start, $gon_side, $current_max) {
  if ($start == count($nums)) {
  $ring = generate_gon_ring($gon_side, $nums);
  if (is_magic_gon($ring)) {
  $s = '';
  $k = 0;
  while ($k < count($ring)) {
  $s = $s . _str($ring[$k]);
  $k = _iadd($k, 1);
};
  if ($s > $current_max) {
  return $s;
};
};
  return $current_max;
}
  $res = $current_max;
  $i = $start;
  while ($i < count($nums)) {
  $swapped = swap($nums, $start, $i);
  $candidate = permute_search($swapped, _iadd($start, 1), $gon_side, $res);
  if ($candidate > $res) {
  $res = $candidate;
}
  $i = _iadd($i, 1);
};
  return $res;
}
function solution($gon_side) {
  if ($gon_side < 3 || $gon_side > 5) {
  return '';
}
  $small = range_desc(_iadd($gon_side, 1), 1);
  $big = range_asc(_iadd($gon_side, 2), _imul($gon_side, 2));
  $numbers = concat_lists($small, $big);
  $max_str = permute_search($numbers, 0, $gon_side, '');
  return $max_str;
}
echo rtrim(solution(5)), PHP_EOL;
