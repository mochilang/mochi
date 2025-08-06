<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$ASCII = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}';
function build_alphabet() {
  global $ASCII, $message, $token, $encoded;
  $result = [];
  $i = 0;
  while ($i < strlen($ASCII)) {
  $result = _append($result, $ASCII[$i]);
  $i = $i + 1;
};
  return $result;
}
function range_list($n) {
  global $ASCII, $message, $token, $encoded;
  $lst = [];
  $i = 0;
  while ($i < $n) {
  $lst = _append($lst, $i);
  $i = $i + 1;
};
  return $lst;
}
function reversed_range_list($n) {
  global $ASCII, $message, $token, $encoded;
  $lst = [];
  $i = $n - 1;
  while ($i >= 0) {
  $lst = _append($lst, $i);
  $i = $i - 1;
};
  return $lst;
}
function index_of_char($lst, $ch) {
  global $ASCII, $message, $token, $encoded;
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function index_of_int($lst, $value) {
  global $ASCII, $message, $token, $encoded;
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] == $value) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function enigma_encrypt($message, $token) {
  global $ASCII, $encoded;
  $alphabets = build_alphabet();
  $n = count($alphabets);
  $gear_one = range_list($n);
  $gear_two = range_list($n);
  $gear_three = range_list($n);
  $reflector = reversed_range_list($n);
  $gear_one_pos = 0;
  $gear_two_pos = 0;
  $gear_three_pos = 0;
  $rotator = null;
$rotator = function() use (&$rotator, $message, $token, $alphabets, $n, &$gear_one, &$gear_two, &$gear_three, $reflector, &$gear_one_pos, &$gear_two_pos, &$gear_three_pos, &$ASCII) {
  $i = $gear_one[0];
  $gear_one = array_slice($gear_one, 1, count($gear_one) - 1);
  $gear_one = _append($gear_one, $i);
  $gear_one_pos = $gear_one_pos + 1;
  if ($gear_one_pos % $n == 0) {
  $i = $gear_two[0];
  $gear_two = array_slice($gear_two, 1, count($gear_two) - 1);
  $gear_two = _append($gear_two, $i);
  $gear_two_pos = $gear_two_pos + 1;
  if ($gear_two_pos % $n == 0) {
  $i = $gear_three[0];
  $gear_three = array_slice($gear_three, 1, count($gear_three) - 1);
  $gear_three = _append($gear_three, $i);
  $gear_three_pos = $gear_three_pos + 1;
};
}
};
  $engine = null;
$engine = function($ch) use (&$engine, $message, $token, $alphabets, $n, $gear_one, $gear_two, $gear_three, $reflector, $gear_one_pos, $gear_two_pos, $gear_three_pos, $rotator, &$ASCII) {
  $target = index_of_char($alphabets, $ch);
  $target = $gear_one[$target];
  $target = $gear_two[$target];
  $target = $gear_three[$target];
  $target = $reflector[$target];
  $target = index_of_int($gear_three, $target);
  $target = index_of_int($gear_two, $target);
  $target = index_of_int($gear_one, $target);
  $rotator();
  return $alphabets[$target];
};
  $t = 0;
  while ($t < $token) {
  $rotator();
  $t = $t + 1;
};
  $result = '';
  $idx = 0;
  while ($idx < strlen($message)) {
  $result = $result . $engine(substr($message, $idx, $idx + 1 - $idx));
  $idx = $idx + 1;
};
  return $result;
}
$message = 'HELLO WORLD';
$token = 123;
$encoded = enigma_encrypt($message, $token);
echo rtrim($encoded), PHP_EOL;
