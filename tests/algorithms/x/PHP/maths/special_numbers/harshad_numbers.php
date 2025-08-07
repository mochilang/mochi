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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  function panic($msg) {
};
  function char_to_value($c) {
  $digits = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $c) {
  return $i;
}
  $i = $i + 1;
};
  panic('invalid digit');
};
  function int_to_base($number, $base) {
  if ($base < 2 || $base > 36) {
  panic('\'base\' must be between 2 and 36 inclusive');
}
  if ($number < 0) {
  panic('number must be a positive integer');
}
  $digits = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $n = $number;
  $result = '';
  while ($n > 0) {
  $remainder = $n % $base;
  $result = substr($digits, $remainder, $remainder + 1 - $remainder) . $result;
  $n = _intdiv($n, $base);
};
  if ($result == '') {
  $result = '0';
}
  return $result;
};
  function base_to_int($num_str, $base) {
  $value = 0;
  $i = 0;
  while ($i < strlen($num_str)) {
  $c = substr($num_str, $i, $i + 1 - $i);
  $value = $value * $base + char_to_value($c);
  $i = $i + 1;
};
  return $value;
};
  function sum_of_digits($num, $base) {
  if ($base < 2 || $base > 36) {
  panic('\'base\' must be between 2 and 36 inclusive');
}
  $num_str = int_to_base($num, $base);
  $total = 0;
  $i = 0;
  while ($i < strlen($num_str)) {
  $c = substr($num_str, $i, $i + 1 - $i);
  $total = $total + char_to_value($c);
  $i = $i + 1;
};
  return int_to_base($total, $base);
};
  function harshad_numbers_in_base($limit, $base) {
  if ($base < 2 || $base > 36) {
  panic('\'base\' must be between 2 and 36 inclusive');
}
  if ($limit < 0) {
  return [];
}
  $numbers = [];
  $i = 1;
  while ($i < $limit) {
  $s = sum_of_digits($i, $base);
  $divisor = base_to_int($s, $base);
  if ($i % $divisor == 0) {
  $numbers = _append($numbers, int_to_base($i, $base));
}
  $i = $i + 1;
};
  return $numbers;
};
  function is_harshad_number_in_base($num, $base) {
  if ($base < 2 || $base > 36) {
  panic('\'base\' must be between 2 and 36 inclusive');
}
  if ($num < 0) {
  return false;
}
  $n = int_to_base($num, $base);
  $d = sum_of_digits($num, $base);
  $n_val = base_to_int($n, $base);
  $d_val = base_to_int($d, $base);
  return $n_val % $d_val == 0;
};
  function main() {
  echo rtrim(int_to_base(0, 21)), PHP_EOL;
  echo rtrim(int_to_base(23, 2)), PHP_EOL;
  echo rtrim(int_to_base(58, 5)), PHP_EOL;
  echo rtrim(int_to_base(167, 16)), PHP_EOL;
  echo rtrim(sum_of_digits(103, 12)), PHP_EOL;
  echo rtrim(sum_of_digits(1275, 4)), PHP_EOL;
  echo rtrim(sum_of_digits(6645, 2)), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(harshad_numbers_in_base(15, 2), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(harshad_numbers_in_base(12, 34), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(harshad_numbers_in_base(12, 4), 1344)))))), PHP_EOL;
  echo rtrim(json_encode(is_harshad_number_in_base(18, 10), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_harshad_number_in_base(21, 10), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_harshad_number_in_base(-21, 5), 1344)), PHP_EOL;
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
