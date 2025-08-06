<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function tribonacci($num) {
  $dp = [];
  $i = 0;
  while ($i < $num) {
  if ($i == 0 || $i == 1) {
  $dp = _append($dp, 0);
} else {
  if ($i == 2) {
  $dp = _append($dp, 1);
} else {
  $t = $dp[$i - 1] + $dp[$i - 2] + $dp[$i - 3];
  $dp = _append($dp, $t);
};
}
  $i = $i + 1;
};
  return $dp;
}
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(tribonacci(8), 1344))))))), PHP_EOL;
