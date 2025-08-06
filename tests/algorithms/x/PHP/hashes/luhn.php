<?php
ini_set('memory_limit', '-1');
function is_luhn($s) {
  $n = strlen($s);
  if ($n <= 1) {
  return false;
}
  $check_digit = intval(substr($s, $n - 1, $n - ($n - 1)));
  $i = $n - 2;
  $even = true;
  while ($i >= 0) {
  $digit = intval(substr($s, $i, $i + 1 - $i));
  if ($even) {
  $doubled = $digit * 2;
  if ($doubled > 9) {
  $doubled = $doubled - 9;
};
  $check_digit = $check_digit + $doubled;
} else {
  $check_digit = $check_digit + $digit;
}
  $even = !$even;
  $i = $i - 1;
};
  return $check_digit % 10 == 0;
}
echo str_replace('    ', '  ', json_encode(is_luhn('79927398713'), 128)), PHP_EOL;
echo str_replace('    ', '  ', json_encode(is_luhn('79927398714'), 128)), PHP_EOL;
