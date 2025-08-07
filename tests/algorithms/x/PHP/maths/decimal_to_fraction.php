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
$__start_mem = memory_get_usage();
$__start = _now();
  function pow10($n) {
  $result = 1;
  $i = 0;
  while ($i < $n) {
  $result = $result * 10;
  $i = $i + 1;
};
  return $result;
};
  function gcd($a, $b) {
  $x = $a;
  $y = $b;
  if ($x < 0) {
  $x = -$x;
}
  if ($y < 0) {
  $y = -$y;
}
  while ($y != 0) {
  $r = $x % $y;
  $x = $y;
  $y = $r;
};
  return $x;
};
  function parse_decimal($s) {
  if (strlen($s) == 0) {
  $panic('invalid number');
}
  $idx = 0;
  $sign = 1;
  $first = substr($s, 0, 1 - 0);
  if ($first == '-') {
  $sign = -1;
  $idx = 1;
} else {
  if ($first == '+') {
  $idx = 1;
};
}
  $int_part = '';
  while ($idx < strlen($s)) {
  $c = substr($s, $idx, $idx + 1 - $idx);
  if ($c >= '0' && $c <= '9') {
  $int_part = $int_part . $c;
  $idx = $idx + 1;
} else {
  break;
}
};
  $frac_part = '';
  if ($idx < strlen($s) && substr($s, $idx, $idx + 1 - $idx) == '.') {
  $idx = $idx + 1;
  while ($idx < strlen($s)) {
  $c = substr($s, $idx, $idx + 1 - $idx);
  if ($c >= '0' && $c <= '9') {
  $frac_part = $frac_part . $c;
  $idx = $idx + 1;
} else {
  break;
}
};
}
  $exp = 0;
  if ($idx < strlen($s) && (substr($s, $idx, $idx + 1 - $idx) == 'e' || substr($s, $idx, $idx + 1 - $idx) == 'E')) {
  $idx = $idx + 1;
  $exp_sign = 1;
  if ($idx < strlen($s) && substr($s, $idx, $idx + 1 - $idx) == '-') {
  $exp_sign = -1;
  $idx = $idx + 1;
} else {
  if ($idx < strlen($s) && substr($s, $idx, $idx + 1 - $idx) == '+') {
  $idx = $idx + 1;
};
};
  $exp_str = '';
  while ($idx < strlen($s)) {
  $c = substr($s, $idx, $idx + 1 - $idx);
  if ($c >= '0' && $c <= '9') {
  $exp_str = $exp_str . $c;
  $idx = $idx + 1;
} else {
  $panic('invalid number');
}
};
  if (strlen($exp_str) == 0) {
  $panic('invalid number');
};
  $exp = $exp_sign * intval($exp_str);
}
  if ($idx != strlen($s)) {
  $panic('invalid number');
}
  if (strlen($int_part) == 0) {
  $int_part = '0';
}
  $num_str = $int_part . $frac_part;
  $numerator = intval($num_str);
  if ($sign == (0 - 1)) {
  $numerator = (0 - $numerator);
}
  $denominator = pow10(strlen($frac_part));
  if ($exp > 0) {
  $numerator = $numerator * pow10($exp);
} else {
  if ($exp < 0) {
  $denominator = $denominator * pow10(-$exp);
};
}
  return ['numerator' => $numerator, 'denominator' => $denominator];
};
  function reduce($fr) {
  $g = gcd($fr['numerator'], $fr['denominator']);
  return ['numerator' => $fr['numerator'] / $g, 'denominator' => $fr['denominator'] / $g];
};
  function decimal_to_fraction_str($s) {
  return reduce(parse_decimal($s));
};
  function decimal_to_fraction($x) {
  return decimal_to_fraction_str(_str($x));
};
  function assert_fraction($name, $fr, $num, $den) {
  if ($fr['numerator'] != $num || $fr['denominator'] != $den) {
  $panic($name);
}
};
  function test_decimal_to_fraction() {
  assert_fraction('case1', decimal_to_fraction(2.0), 2, 1);
  assert_fraction('case2', decimal_to_fraction(89.0), 89, 1);
  assert_fraction('case3', decimal_to_fraction_str('67'), 67, 1);
  assert_fraction('case4', decimal_to_fraction_str('45.0'), 45, 1);
  assert_fraction('case5', decimal_to_fraction(1.5), 3, 2);
  assert_fraction('case6', decimal_to_fraction_str('6.25'), 25, 4);
  assert_fraction('case7', decimal_to_fraction(0.0), 0, 1);
  assert_fraction('case8', decimal_to_fraction(-2.5), -5, 2);
  assert_fraction('case9', decimal_to_fraction(0.125), 1, 8);
  assert_fraction('case10', decimal_to_fraction(1000000.25), 4000001, 4);
  assert_fraction('case11', decimal_to_fraction(1.3333), 13333, 10000);
  assert_fraction('case12', decimal_to_fraction_str('1.23e2'), 123, 1);
  assert_fraction('case13', decimal_to_fraction_str('0.500'), 1, 2);
};
  function main() {
  test_decimal_to_fraction();
  $fr = decimal_to_fraction(1.5);
  echo rtrim(_str($fr['numerator']) . '/' . _str($fr['denominator'])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
