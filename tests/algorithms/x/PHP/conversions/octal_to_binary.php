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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function octal_to_binary($octal_number) {
  if (strlen($octal_number) == 0) {
  $panic('Empty string was passed to the function');
}
  $octal_digits = '01234567';
  $binary_number = '';
  $i = 0;
  while ($i < strlen($octal_number)) {
  $digit = substr($octal_number, $i, $i + 1 - $i);
  $valid = false;
  $j = 0;
  while ($j < strlen($octal_digits)) {
  if ($digit == substr($octal_digits, $j, $j + 1 - $j)) {
  $valid = true;
  break;
}
  $j = $j + 1;
};
  if (!$valid) {
  $panic('Non-octal value was passed to the function');
}
  $value = intval($digit);
  $k = 0;
  $binary_digit = '';
  while ($k < 3) {
  $binary_digit = _str($value % 2) . $binary_digit;
  $value = _intdiv($value, 2);
  $k = $k + 1;
};
  $binary_number = $binary_number . $binary_digit;
  $i = $i + 1;
};
  return $binary_number;
};
  echo rtrim(octal_to_binary('17')), PHP_EOL;
  echo rtrim(octal_to_binary('7')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
