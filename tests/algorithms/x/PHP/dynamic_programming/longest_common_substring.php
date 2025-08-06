<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function longest_common_substring($text1, $text2) {
  if (strlen($text1) == 0 || strlen($text2) == 0) {
  return '';
}
  $m = strlen($text1);
  $n = strlen($text2);
  $dp = [];
  $i = 0;
  while ($i < $m + 1) {
  $row = [];
  $j = 0;
  while ($j < $n + 1) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $dp = _append($dp, $row);
  $i = $i + 1;
};
  $end_pos = 0;
  $max_len = 0;
  $ii = 1;
  while ($ii <= $m) {
  $jj = 1;
  while ($jj <= $n) {
  if (substr($text1, $ii - 1, $ii - ($ii - 1)) == substr($text2, $jj - 1, $jj - ($jj - 1))) {
  $dp[$ii][$jj] = 1 + $dp[$ii - 1][$jj - 1];
  if ($dp[$ii][$jj] > $max_len) {
  $max_len = $dp[$ii][$jj];
  $end_pos = $ii;
};
}
  $jj = $jj + 1;
};
  $ii = $ii + 1;
};
  return substr($text1, $end_pos - $max_len, $end_pos - ($end_pos - $max_len));
}
echo rtrim(longest_common_substring('abcdef', 'xabded')), PHP_EOL;
echo rtrim('
'), PHP_EOL;
echo rtrim(longest_common_substring('zxabcdezy', 'yzabcdezx')), PHP_EOL;
