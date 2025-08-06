<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function make_list($len, $value) {
  $arr = [];
  $i = 0;
  while ($i < $len) {
  $arr = _append($arr, $value);
  $i = $i + 1;
};
  return $arr;
}
function int_sqrt($n) {
  $r = 0;
  while (($r + 1) * ($r + 1) <= $n) {
  $r = $r + 1;
};
  return $r;
}
function minimum_squares_to_represent_a_number($number) {
  if ($number < 0) {
  $panic('the value of input must not be a negative number');
}
  if ($number == 0) {
  return 1;
}
  $answers = make_list($number + 1, -1);
  $answers[0] = 0;
  $i = 1;
  while ($i <= $number) {
  $answer = $i;
  $root = int_sqrt($i);
  $j = 1;
  while ($j <= $root) {
  $current_answer = 1 + $answers[$i - $j * $j];
  if ($current_answer < $answer) {
  $answer = $current_answer;
}
  $j = $j + 1;
};
  $answers[$i] = $answer;
  $i = $i + 1;
};
  return $answers[$number];
}
echo rtrim(json_encode(minimum_squares_to_represent_a_number(25), 1344)), PHP_EOL;
echo rtrim(json_encode(minimum_squares_to_represent_a_number(21), 1344)), PHP_EOL;
