<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
$__start_mem = memory_get_usage();
$__start = _now();
  function validate_initial_digits($cc) {
  return substr($cc, 0, 2) == '34' || substr($cc, 0, 2) == '35' || substr($cc, 0, 2) == '37' || substr($cc, 0, 1) == '4' || substr($cc, 0, 1) == '5' || substr($cc, 0, 1) == '6';
};
  function luhn_validation($cc) {
  $sum = 0;
  $double_digit = false;
  $i = strlen($cc) - 1;
  while ($i >= 0) {
  $n = (ctype_digit($cc[$i]) ? intval($cc[$i]) : ord($cc[$i]));
  if ($double_digit) {
  $n = $n * 2;
  if ($n > 9) {
  $n = $n - 9;
};
}
  $sum = $sum + $n;
  $double_digit = !$double_digit;
  $i = $i - 1;
};
  return $sum % 10 == 0;
};
  function is_digit_string($s) {
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function validate_credit_card_number($cc) {
  $error_message = $cc . ' is an invalid credit card number because';
  if (!is_digit_string($cc)) {
  echo rtrim($error_message . ' it has nonnumerical characters.'), PHP_EOL;
  return false;
}
  if (!(strlen($cc) >= 13 && strlen($cc) <= 16)) {
  echo rtrim($error_message . ' of its length.'), PHP_EOL;
  return false;
}
  if (!validate_initial_digits($cc)) {
  echo rtrim($error_message . ' of its first two digits.'), PHP_EOL;
  return false;
}
  if (!luhn_validation($cc)) {
  echo rtrim($error_message . ' it fails the Luhn check.'), PHP_EOL;
  return false;
}
  echo rtrim($cc . ' is a valid credit card number.'), PHP_EOL;
  return true;
};
  function main() {
  validate_credit_card_number('4111111111111111');
  validate_credit_card_number('32323');
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
