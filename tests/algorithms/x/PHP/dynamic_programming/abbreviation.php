<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function index_of($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function mochi_ord($ch) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = index_of($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = index_of($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  return 0;
}
function mochi_chr($n) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - ($n - 65));
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - ($n - 97));
}
  return '?';
}
function to_upper_char($c) {
  $code = mochi_ord($c);
  if ($code >= 97 && $code <= 122) {
  return mochi_chr($code - 32);
}
  return $c;
}
function is_lower($c) {
  $code = mochi_ord($c);
  return $code >= 97 && $code <= 122;
}
function abbr($a, $b) {
  $n = strlen($a);
  $m = strlen($b);
  $dp = [];
  $i = 0;
  while ($i <= $n) {
  $row = [];
  $j = 0;
  while ($j <= $m) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $dp = _append($dp, $row);
  $i = $i + 1;
};
  $dp[0][0] = true;
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j <= $m) {
  if ($dp[$i][$j]) {
  if ($j < $m && to_upper_char(substr($a, $i, $i + 1 - $i)) == substr($b, $j, $j + 1 - $j)) {
  $dp[$i + 1][$j + 1] = true;
};
  if (is_lower(substr($a, $i, $i + 1 - $i))) {
  $dp[$i + 1][$j] = true;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $dp[$n][$m];
}
function print_bool($b) {
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
}
print_bool(abbr('daBcd', 'ABC'));
print_bool(abbr('dBcd', 'ABC'));
