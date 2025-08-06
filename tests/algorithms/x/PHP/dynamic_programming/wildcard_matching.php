<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function make_bool_list($n) {
  $row = [];
  $i = 0;
  while ($i < $n) {
  $row = _append($row, false);
  $i = $i + 1;
};
  return $row;
}
function make_bool_matrix($rows, $cols) {
  $matrix = [];
  $i = 0;
  while ($i < $rows) {
  $matrix = _append($matrix, make_bool_list($cols));
  $i = $i + 1;
};
  return $matrix;
}
function is_match($s, $p) {
  $n = strlen($s);
  $m = strlen($p);
  $dp = make_bool_matrix($n + 1, $m + 1);
  $dp[0][0] = true;
  $j = 1;
  while ($j <= $m) {
  if (substr($p, $j - 1, $j - ($j - 1)) == '*') {
  $dp[0][$j] = $dp[0][$j - 1];
}
  $j = $j + 1;
};
  $i = 1;
  while ($i <= $n) {
  $j2 = 1;
  while ($j2 <= $m) {
  $pc = substr($p, $j2 - 1, $j2 - ($j2 - 1));
  $sc = substr($s, $i - 1, $i - ($i - 1));
  if ($pc == $sc || $pc == '?') {
  $dp[$i][$j2] = $dp[$i - 1][$j2 - 1];
} else {
  if ($pc == '*') {
  if ($dp[$i - 1][$j2] || $dp[$i][$j2 - 1]) {
  $dp[$i][$j2] = true;
};
};
}
  $j2 = $j2 + 1;
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
print_bool(is_match('abc', 'a*c'));
print_bool(is_match('abc', 'a*d'));
print_bool(is_match('baaabab', '*****ba*****ab'));
