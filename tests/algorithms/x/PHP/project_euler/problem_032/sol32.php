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
function join_digits($xs) {
  global $digits, $products, $total;
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . $xs[$i];
  $i = _iadd($i, 1);
};
  return $s;
}
function digits_to_int($xs) {
  global $digits, $products, $total, $i;
  return intval(join_digits($xs));
}
function contains_int($xs, $value) {
  global $digits, $products, $total;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function remove_at($xs, $idx) {
  global $digits, $products, $total;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function is_combination_valid($comb) {
  global $digits, $products, $total, $i;
  $prod = digits_to_int(array_slice($comb, 5, 9 - 5));
  $mul2 = digits_to_int(array_slice($comb, 0, 2));
  $mul3 = digits_to_int(array_slice($comb, 2, 5 - 2));
  if (_imul($mul2, $mul3) == $prod) {
  return true;
}
  $mul1 = digits_to_int(array_slice($comb, 0, 1));
  $mul4 = digits_to_int(array_slice($comb, 1, 5 - 1));
  return _imul($mul1, $mul4) == $prod;
}
function search($prefix, $remaining, $products) {
  global $digits, $total;
  if (count($remaining) == 0) {
  if (is_combination_valid($prefix)) {
  $p = digits_to_int(array_slice($prefix, 5, 9 - 5));
  if (!contains_int($products, $p)) {
  $products = _append($products, $p);
};
};
  return $products;
}
  $i = 0;
  while ($i < count($remaining)) {
  $next_prefix = _append($prefix, $remaining[$i]);
  $next_remaining = remove_at($remaining, $i);
  $products = search($next_prefix, $next_remaining, $products);
  $i = _iadd($i, 1);
};
  return $products;
}
$digits = ['1', '2', '3', '4', '5', '6', '7', '8', '9'];
$products = [];
$products = search([], $digits, $products);
$total = 0;
$i = 0;
while ($i < count($products)) {
  $total = _iadd($total, $products[$i]);
  $i = _iadd($i, 1);
}
echo rtrim(_str($total)), PHP_EOL;
