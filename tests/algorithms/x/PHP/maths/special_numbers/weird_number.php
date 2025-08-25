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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function bubble_sort($xs) {
  $arr = $xs;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - $i - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function factors($num) {
  $values = [1];
  $i = 2;
  while ($i * $i <= $num) {
  if ($num % $i == 0) {
  $values = _append($values, $i);
  $d = _intdiv($num, $i);
  if ($d != $i) {
  $values = _append($values, $d);
};
}
  $i = $i + 1;
};
  return bubble_sort($values);
}
function sum_list($xs) {
  $total = 0;
  $i = 0;
  while ($i < count($xs)) {
  $total = $total + $xs[$i];
  $i = $i + 1;
};
  return $total;
}
function abundant($n) {
  return sum_list(factors($n)) > $n;
}
function semi_perfect($number) {
  if ($number <= 0) {
  return true;
}
  $values = factors($number);
  $possible = [];
  $j = 0;
  while ($j <= $number) {
  $possible = _append($possible, $j == 0);
  $j = $j + 1;
};
  $idx = 0;
  while ($idx < count($values)) {
  $v = $values[$idx];
  $s = $number;
  while ($s >= $v) {
  if ($possible[$s - $v]) {
  $possible[$s] = true;
}
  $s = $s - 1;
};
  $idx = $idx + 1;
};
  return $possible[$number];
}
function weird($number) {
  return abundant($number) && semi_perfect($number) == false;
}
function run_tests() {
  if (factors(12) != [1, 2, 3, 4, 6]) {
  _panic('factors 12 failed');
}
  if (factors(1) != [1]) {
  _panic('factors 1 failed');
}
  if (factors(100) != [1, 2, 4, 5, 10, 20, 25, 50]) {
  _panic('factors 100 failed');
}
  if (abundant(0) != true) {
  _panic('abundant 0 failed');
}
  if (abundant(1) != false) {
  _panic('abundant 1 failed');
}
  if (abundant(12) != true) {
  _panic('abundant 12 failed');
}
  if (abundant(13) != false) {
  _panic('abundant 13 failed');
}
  if (abundant(20) != true) {
  _panic('abundant 20 failed');
}
  if (semi_perfect(0) != true) {
  _panic('semi_perfect 0 failed');
}
  if (semi_perfect(1) != true) {
  _panic('semi_perfect 1 failed');
}
  if (semi_perfect(12) != true) {
  _panic('semi_perfect 12 failed');
}
  if (semi_perfect(13) != false) {
  _panic('semi_perfect 13 failed');
}
  if (weird(0) != false) {
  _panic('weird 0 failed');
}
  if (weird(70) != true) {
  _panic('weird 70 failed');
}
  if (weird(77) != false) {
  _panic('weird 77 failed');
}
}
function main() {
  run_tests();
  $nums = [69, 70, 71];
  $i = 0;
  while ($i < count($nums)) {
  $n = $nums[$i];
  if (weird($n)) {
  echo rtrim(_str($n) . ' is weird.'), PHP_EOL;
} else {
  echo rtrim(_str($n) . ' is not weird.'), PHP_EOL;
}
  $i = $i + 1;
};
}
main();
