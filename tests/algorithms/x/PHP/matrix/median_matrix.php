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
function bubble_sort($a) {
  global $matrix1, $matrix2;
  $arr = $a;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j + 1 < $n - $i) {
  if ($arr[$j] > $arr[$j + 1]) {
  $temp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $temp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function median($matrix) {
  global $matrix1, $matrix2;
  $linear = [];
  $i = 0;
  while ($i < count($matrix)) {
  $row = $matrix[$i];
  $j = 0;
  while ($j < count($row)) {
  $linear = _append($linear, $row[$j]);
  $j = $j + 1;
};
  $i = $i + 1;
};
  $sorted = bubble_sort($linear);
  $mid = _intdiv((count($sorted) - 1), 2);
  return $sorted[$mid];
}
$matrix1 = [[1, 3, 5], [2, 6, 9], [3, 6, 9]];
echo rtrim(_str(median($matrix1))), PHP_EOL;
$matrix2 = [[1, 2, 3], [4, 5, 6]];
echo rtrim(_str(median($matrix2))), PHP_EOL;
