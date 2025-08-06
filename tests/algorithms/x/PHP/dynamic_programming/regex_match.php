<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function recursive_match($text, $pattern) {
  if (strlen($pattern) == 0) {
  return strlen($text) == 0;
}
  if (strlen($text) == 0) {
  if (strlen($pattern) >= 2 && substr($pattern, strlen($pattern) - 1, strlen($pattern) - (strlen($pattern) - 1)) == '*') {
  return recursive_match($text, substr($pattern, 0, strlen($pattern) - 2 - 0));
};
  return false;
}
  $last_text = substr($text, strlen($text) - 1, strlen($text) - (strlen($text) - 1));
  $last_pattern = substr($pattern, strlen($pattern) - 1, strlen($pattern) - (strlen($pattern) - 1));
  if ($last_text == $last_pattern || $last_pattern == '.') {
  return recursive_match(substr($text, 0, strlen($text) - 1 - 0), substr($pattern, 0, strlen($pattern) - 1 - 0));
}
  if ($last_pattern == '*') {
  if (recursive_match(substr($text, 0, strlen($text) - 1 - 0), $pattern)) {
  return true;
};
  return recursive_match($text, substr($pattern, 0, strlen($pattern) - 2 - 0));
}
  return false;
}
function dp_match($text, $pattern) {
  $m = strlen($text);
  $n = strlen($pattern);
  $dp = [];
  $i = 0;
  while ($i <= $m) {
  $row = [];
  $j = 0;
  while ($j <= $n) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $dp = _append($dp, $row);
  $i = $i + 1;
};
  $dp[0][0] = true;
  $j = 1;
  while ($j <= $n) {
  if (substr($pattern, $j - 1, $j - ($j - 1)) == '*' && $j >= 2) {
  if ($dp[0][$j - 2]) {
  $dp[0][$j] = true;
};
}
  $j = $j + 1;
};
  $i = 1;
  while ($i <= $m) {
  $j = 1;
  while ($j <= $n) {
  $p_char = substr($pattern, $j - 1, $j - ($j - 1));
  $t_char = substr($text, $i - 1, $i - ($i - 1));
  if ($p_char == '.' || $p_char == $t_char) {
  if ($dp[$i - 1][$j - 1]) {
  $dp[$i][$j] = true;
};
} else {
  if ($p_char == '*') {
  if ($j >= 2) {
  if ($dp[$i][$j - 2]) {
  $dp[$i][$j] = true;
};
  $prev_p = substr($pattern, $j - 2, $j - 1 - ($j - 2));
  if ($prev_p == '.' || $prev_p == $t_char) {
  if ($dp[$i - 1][$j]) {
  $dp[$i][$j] = true;
};
};
};
} else {
  $dp[$i][$j] = false;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $dp[$m][$n];
}
function print_bool($b) {
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
}
print_bool(recursive_match('abc', 'a.c'));
print_bool(recursive_match('abc', 'af*.c'));
print_bool(recursive_match('abc', 'a.c*'));
print_bool(recursive_match('abc', 'a.c*d'));
print_bool(recursive_match('aa', '.*'));
print_bool(dp_match('abc', 'a.c'));
print_bool(dp_match('abc', 'af*.c'));
print_bool(dp_match('abc', 'a.c*'));
print_bool(dp_match('abc', 'a.c*d'));
print_bool(dp_match('aa', '.*'));
