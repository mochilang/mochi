<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function repeat_char($ch, $count) {
  $result = '';
  $i = 0;
  while ($i < $count) {
  $result = $result . $ch;
  $i = $i + 1;
};
  return $result;
}
function butterfly_pattern($n) {
  $lines = [];
  $i = 1;
  while ($i < $n) {
  $left = repeat_char('*', $i);
  $mid = repeat_char(' ', 2 * ($n - $i) - 1);
  $right = repeat_char('*', $i);
  $lines = _append($lines, $left . $mid . $right);
  $i = $i + 1;
};
  $lines = _append($lines, repeat_char('*', 2 * $n - 1));
  $j = $n - 1;
  while ($j > 0) {
  $left = repeat_char('*', $j);
  $mid = repeat_char(' ', 2 * ($n - $j) - 1);
  $right = repeat_char('*', $j);
  $lines = _append($lines, $left . $mid . $right);
  $j = $j - 1;
};
  $out = '';
  $k = 0;
  while ($k < count($lines)) {
  if ($k > 0) {
  $out = $out . '
';
}
  $out = $out . $lines[$k];
  $k = $k + 1;
};
  return $out;
}
echo rtrim(butterfly_pattern(3)), PHP_EOL;
echo rtrim(butterfly_pattern(5)), PHP_EOL;
