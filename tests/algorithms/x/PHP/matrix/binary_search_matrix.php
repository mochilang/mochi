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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function binary_search($arr, $lower_bound, $upper_bound, $value) {
  $r = _intdiv(($lower_bound + $upper_bound), 2);
  if ($arr[$r] == $value) {
  return $r;
}
  if ($lower_bound >= $upper_bound) {
  return -1;
}
  if ($arr[$r] < $value) {
  return binary_search($arr, $r + 1, $upper_bound, $value);
}
  return binary_search($arr, $lower_bound, $r - 1, $value);
}
function mat_bin_search($value, $matrix) {
  $index = 0;
  if ($matrix[$index][0] == $value) {
  return [$index, 0];
}
  while ($index < count($matrix) && $matrix[$index][0] < $value) {
  $r = binary_search($matrix[$index], 0, count($matrix[$index]) - 1, $value);
  if ($r != (-1)) {
  return [$index, $r];
}
  $index = $index + 1;
};
  return [-1, -1];
}
function main() {
  $row = [1, 4, 7, 11, 15];
  echo rtrim(_str(binary_search($row, 0, count($row) - 1, 1))), PHP_EOL;
  echo rtrim(_str(binary_search($row, 0, count($row) - 1, 23))), PHP_EOL;
  $matrix = [[1, 4, 7, 11, 15], [2, 5, 8, 12, 19], [3, 6, 9, 16, 22], [10, 13, 14, 17, 24], [18, 21, 23, 26, 30]];
  echo rtrim(_str(mat_bin_search(1, $matrix))), PHP_EOL;
  echo rtrim(_str(mat_bin_search(34, $matrix))), PHP_EOL;
}
main();
