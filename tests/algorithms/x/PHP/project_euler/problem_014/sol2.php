<?php
ini_set('memory_limit', '-1');
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
$collatz_cache = [1 => 1];
function collatz_length($n) {
  global $collatz_cache, $input_str, $limit;
  $num = $n;
  $sequence = [];
  while (!(array_key_exists($num, $collatz_cache))) {
  $sequence = _append($sequence, $num);
  if ($num % 2 == 0) {
  $num = intval((_intdiv($num, 2)));
} else {
  $num = 3 * $num + 1;
}
};
  $length = $collatz_cache[$num];
  $i = count($sequence) - 1;
  while ($i >= 0) {
  $length = $length + 1;
  $collatz_cache[$sequence[$i]] = $length;
  $i = $i - 1;
};
  return $length;
}
function solution($limit) {
  global $collatz_cache, $input_str;
  $max_len = 0;
  $max_start = 1;
  $i = 1;
  while ($i < $limit) {
  $length = collatz_length($i);
  if ($length > $max_len) {
  $max_len = $length;
  $max_start = $i;
}
  $i = $i + 1;
};
  return $max_start;
}
$input_str = trim(fgets(STDIN));
$limit = intval($input_str);
echo rtrim(json_encode(solution($limit), 1344)), PHP_EOL;
