<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
$__start_mem = memory_get_usage();
$__start = _now();
  function parse_row($row_str) {
  global $MATRIX_2, $result;
  $nums = [];
  $current = 0;
  $has_digit = false;
  $i = 0;
  while ($i < strlen($row_str)) {
  $ch = substr($row_str, $i, _iadd($i, 1) - $i);
  if ($ch == ' ') {
  if ($has_digit) {
  $nums = _append($nums, $current);
  $current = 0;
  $has_digit = false;
};
} else {
  $current = _iadd(_imul($current, 10), (intval($ch)));
  $has_digit = true;
}
  $i = _iadd($i, 1);
};
  if ($has_digit) {
  $nums = _append($nums, $current);
}
  return $nums;
};
  function parse_matrix($matrix_str) {
  global $MATRIX_2, $result;
  $matrix = [];
  foreach ($matrix_str as $row_str) {
  $row = parse_row($row_str);
  $matrix = _append($matrix, $row);
};
  return $matrix;
};
  function bitcount($x) {
  global $MATRIX_2, $result;
  $count = 0;
  $y = $x;
  while ($y > 0) {
  if (_imod($y, 2) == 1) {
  $count = _iadd($count, 1);
}
  $y = _intdiv($y, 2);
};
  return $count;
};
  function build_powers($n) {
  global $MATRIX_2, $result;
  $powers = [];
  $i = 0;
  $current = 1;
  while ($i <= $n) {
  $powers = _append($powers, $current);
  $current = _imul($current, 2);
  $i = _iadd($i, 1);
};
  return $powers;
};
  function solution($matrix_str) {
  global $MATRIX_2, $result;
  $arr = parse_matrix($matrix_str);
  $n = count($arr);
  $powers = build_powers($n);
  $size = $powers[$n];
  $dp = [];
  $i = 0;
  while ($i < $size) {
  $dp = _append($dp, 0);
  $i = _iadd($i, 1);
};
  $mask = 0;
  while ($mask < $size) {
  $row = bitcount($mask);
  if ($row < $n) {
  $col = 0;
  while ($col < $n) {
  if (_imod((_idiv($mask, $powers[$col])), 2) == 0) {
  $new_mask = _iadd($mask, $powers[$col]);
  $value = _iadd($dp[$mask], $arr[$row][$col]);
  if ($value > $dp[$new_mask]) {
  $dp[$new_mask] = $value;
};
}
  $col = _iadd($col, 1);
};
}
  $mask = _iadd($mask, 1);
};
  return $dp[_isub($size, 1)];
};
  $MATRIX_2 = ['7 53 183 439 863 497 383 563 79 973 287 63 343 169 583', '627 343 773 959 943 767 473 103 699 303 957 703 583 639 913', '447 283 463 29 23 487 463 993 119 883 327 493 423 159 743', '217 623 3 399 853 407 103 983 89 463 290 516 212 462 350', '960 376 682 962 300 780 486 502 912 800 250 346 172 812 350', '870 456 192 162 593 473 915 45 989 873 823 965 425 329 803', '973 965 905 919 133 673 665 235 509 613 673 815 165 992 326', '322 148 972 962 286 255 941 541 265 323 925 281 601 95 973', '445 721 11 525 473 65 511 164 138 672 18 428 154 448 848', '414 456 310 312 798 104 566 520 302 248 694 976 430 392 198', '184 829 373 181 631 101 969 613 840 740 778 458 284 760 390', '821 461 843 513 17 901 711 993 293 157 274 94 192 156 574', '34 124 4 878 450 476 712 914 838 669 875 299 823 329 699', '815 559 813 459 522 788 168 586 966 232 308 833 251 631 107', '813 883 451 509 615 77 281 613 459 205 380 274 302 35 805'];
  $result = solution($MATRIX_2);
  echo rtrim('solution() = ' . _str($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
