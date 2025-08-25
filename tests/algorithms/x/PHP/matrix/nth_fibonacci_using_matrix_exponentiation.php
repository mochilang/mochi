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
function multiply($matrix_a, $matrix_b) {
  $n = count($matrix_a);
  $matrix_c = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $val = 0;
  $k = 0;
  while ($k < $n) {
  $val = $val + $matrix_a[$i][$k] * $matrix_b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $val);
  $j = $j + 1;
};
  $matrix_c = _append($matrix_c, $row);
  $i = $i + 1;
};
  return $matrix_c;
}
function identity($n) {
  $res = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 1);
} else {
  $row = _append($row, 0);
}
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
}
function nth_fibonacci_matrix($n) {
  if ($n <= 1) {
  return $n;
}
  $res_matrix = identity(2);
  $fib_matrix = [[1, 1], [1, 0]];
  $m = $n - 1;
  while ($m > 0) {
  if ($m % 2 == 1) {
  $res_matrix = multiply($res_matrix, $fib_matrix);
}
  $fib_matrix = multiply($fib_matrix, $fib_matrix);
  $m = _intdiv($m, 2);
};
  return $res_matrix[0][0];
}
function nth_fibonacci_bruteforce($n) {
  if ($n <= 1) {
  return $n;
}
  $fib0 = 0;
  $fib1 = 1;
  $i = 2;
  while ($i <= $n) {
  $next = $fib0 + $fib1;
  $fib0 = $fib1;
  $fib1 = $next;
  $i = $i + 1;
};
  return $fib1;
}
function parse_number($s) {
  $result = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch >= '0' && $ch <= '9') {
  $result = $result * 10 + (intval($ch));
}
  $i = $i + 1;
};
  return $result;
}
function main() {
  $ordinals = ['0th', '1st', '2nd', '3rd', '10th', '100th', '1000th'];
  $i = 0;
  while ($i < count($ordinals)) {
  $ordinal = $ordinals[$i];
  $n = parse_number($ordinal);
  $msg = $ordinal . ' fibonacci number using matrix exponentiation is ' . _str(nth_fibonacci_matrix($n)) . ' and using bruteforce is ' . _str(nth_fibonacci_bruteforce($n));
  echo rtrim($msg), PHP_EOL;
  $i = $i + 1;
};
}
main();
