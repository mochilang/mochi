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
  $ones = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'];
  $teens = ['ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
  $tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
  $short_powers = [15, 12, 9, 6, 3, 2];
  $short_units = ['quadrillion', 'trillion', 'billion', 'million', 'thousand', 'hundred'];
  $long_powers = [15, 9, 6, 3, 2];
  $long_units = ['billiard', 'milliard', 'million', 'thousand', 'hundred'];
  $indian_powers = [14, 12, 7, 5, 3, 2];
  $indian_units = ['crore crore', 'lakh crore', 'crore', 'lakh', 'thousand', 'hundred'];
  function pow10($exp) {
  global $ones, $teens, $tens, $short_powers, $short_units, $long_powers, $long_units, $indian_powers, $indian_units;
  $res = 1;
  $i = 0;
  while ($i < $exp) {
  $res = $res * 10;
  $i = $i + 1;
};
  return $res;
};
  function max_value($system) {
  global $ones, $teens, $tens, $short_powers, $short_units, $long_powers, $long_units, $indian_powers, $indian_units;
  if ($system == 'short') {
  return pow10(18) - 1;
}
  if ($system == 'long') {
  return pow10(21) - 1;
}
  if ($system == 'indian') {
  return pow10(19) - 1;
}
  return 0;
};
  function join_words($words) {
  global $ones, $teens, $tens, $short_powers, $short_units, $long_powers, $long_units, $indian_powers, $indian_units;
  $res = '';
  $i = 0;
  while ($i < count($words)) {
  if ($i > 0) {
  $res = $res . ' ';
}
  $res = $res . $words[$i];
  $i = $i + 1;
};
  return $res;
};
  function convert_small_number($num) {
  global $ones, $teens, $tens, $short_powers, $short_units, $long_powers, $long_units, $indian_powers, $indian_units;
  if ($num < 0) {
  return '';
}
  if ($num >= 100) {
  return '';
}
  $tens_digit = _intdiv($num, 10);
  $ones_digit = $num % 10;
  if ($tens_digit == 0) {
  return $ones[$ones_digit];
}
  if ($tens_digit == 1) {
  return $teens[$ones_digit];
}
  $hyphen = ($ones_digit > 0 ? '-' : '');
  $tail = ($ones_digit > 0 ? $ones[$ones_digit] : '');
  return $tens[$tens_digit] . $hyphen . $tail;
};
  function convert_number($num, $system) {
  global $ones, $teens, $tens, $short_powers, $short_units, $long_powers, $long_units, $indian_powers, $indian_units;
  $word_groups = [];
  $n = $num;
  if ($n < 0) {
  $word_groups = _append($word_groups, 'negative');
  $n = -$n;
}
  if ($n > max_value($system)) {
  return '';
}
  $powers = [];
  $units = [];
  if ($system == 'short') {
  $powers = $short_powers;
  $units = $short_units;
} else {
  if ($system == 'long') {
  $powers = $long_powers;
  $units = $long_units;
} else {
  if ($system == 'indian') {
  $powers = $indian_powers;
  $units = $indian_units;
} else {
  return '';
};
};
}
  $i = 0;
  while ($i < count($powers)) {
  $power = $powers[$i];
  $unit = $units[$i];
  $divisor = pow10($power);
  $digit_group = _intdiv($n, $divisor);
  $n = $n % $divisor;
  if ($digit_group > 0) {
  $word_group = ($digit_group >= 100 ? convert_number($digit_group, $system) : convert_small_number($digit_group));
  $word_groups = _append($word_groups, $word_group . ' ' . $unit);
}
  $i = $i + 1;
};
  if ($n > 0 || count($word_groups) == 0) {
  $word_groups = _append($word_groups, convert_small_number($n));
}
  $joined = join_words($word_groups);
  return $joined;
};
  echo rtrim(convert_number(123456789012345, 'short')), PHP_EOL;
  echo rtrim(convert_number(123456789012345, 'long')), PHP_EOL;
  echo rtrim(convert_number(123456789012345, 'indian')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
