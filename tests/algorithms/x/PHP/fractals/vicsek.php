<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function repeat_char($c, $count) {
  $s = '';
  $i = 0;
  while ($i < $count) {
  $s = $s . $c;
  $i = $i + 1;
};
  return $s;
}
function vicsek($order) {
  if ($order == 0) {
  return ['#'];
}
  $prev = vicsek($order - 1);
  $size = count($prev);
  $blank = repeat_char(' ', $size);
  $result = [];
  $i = 0;
  while ($i < $size) {
  $result = _append($result, $blank . $prev[$i] . $blank);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $size) {
  $result = _append($result, $prev[$i] . $prev[$i] . $prev[$i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $size) {
  $result = _append($result, $blank . $prev[$i] . $blank);
  $i = $i + 1;
};
  return $result;
}
function print_pattern($pattern) {
  $i = 0;
  while ($i < count($pattern)) {
  echo rtrim($pattern[$i]), PHP_EOL;
  $i = $i + 1;
};
}
function main() {
  $depth = 3;
  $pattern = vicsek($depth);
  print_pattern($pattern);
}
main();
