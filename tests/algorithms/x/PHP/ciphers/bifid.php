<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$SQUARE = [['a', 'b', 'c', 'd', 'e'], ['f', 'g', 'h', 'i', 'k'], ['l', 'm', 'n', 'o', 'p'], ['q', 'r', 's', 't', 'u'], ['v', 'w', 'x', 'y', 'z']];
function index_of($s, $ch) {
  global $SQUARE;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function to_lower_without_spaces($message, $replace_j) {
  global $SQUARE;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $res = '';
  $i = 0;
  while ($i < strlen($message)) {
  $ch = substr($message, $i, $i + 1 - $i);
  $pos = index_of($upper, $ch);
  if ($pos >= 0) {
  $ch = substr($lower, $pos, $pos + 1 - $pos);
}
  if ($ch != ' ') {
  if ($replace_j && $ch == 'j') {
  $ch = 'i';
};
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
}
function letter_to_numbers($letter) {
  global $SQUARE;
  $r = 0;
  while ($r < count($SQUARE)) {
  $c = 0;
  while ($c < count($SQUARE[$r])) {
  if ($SQUARE[$r][$c] == $letter) {
  return [$r + 1, $c + 1];
}
  $c = $c + 1;
};
  $r = $r + 1;
};
  return [0, 0];
}
function numbers_to_letter($row, $col) {
  global $SQUARE;
  return $SQUARE[$row - 1][$col - 1];
}
function encode($message) {
  global $SQUARE;
  $clean = to_lower_without_spaces($message, true);
  $l = strlen($clean);
  $rows = [];
  $cols = [];
  $i = 0;
  while ($i < $l) {
  $nums = letter_to_numbers(substr($clean, $i, $i + 1 - $i));
  $rows = _append($rows, $nums[0]);
  $cols = _append($cols, $nums[1]);
  $i = $i + 1;
};
  $seq = [];
  $i = 0;
  while ($i < $l) {
  $seq = _append($seq, $rows[$i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $l) {
  $seq = _append($seq, $cols[$i]);
  $i = $i + 1;
};
  $encoded = '';
  $i = 0;
  while ($i < $l) {
  $r = $seq[2 * $i];
  $c = $seq[2 * $i + 1];
  $encoded = $encoded . numbers_to_letter($r, $c);
  $i = $i + 1;
};
  return $encoded;
}
function decode($message) {
  global $SQUARE;
  $clean = to_lower_without_spaces($message, false);
  $l = strlen($clean);
  $first = [];
  $i = 0;
  while ($i < $l) {
  $nums = letter_to_numbers(substr($clean, $i, $i + 1 - $i));
  $first = _append($first, $nums[0]);
  $first = _append($first, $nums[1]);
  $i = $i + 1;
};
  $top = [];
  $bottom = [];
  $i = 0;
  while ($i < $l) {
  $top = _append($top, $first[$i]);
  $bottom = _append($bottom, $first[$i + $l]);
  $i = $i + 1;
};
  $decoded = '';
  $i = 0;
  while ($i < $l) {
  $r = $top[$i];
  $c = $bottom[$i];
  $decoded = $decoded . numbers_to_letter($r, $c);
  $i = $i + 1;
};
  return $decoded;
}
echo rtrim(encode('testmessage')), PHP_EOL;
echo rtrim(encode('Test Message')), PHP_EOL;
echo rtrim(encode('test j')), PHP_EOL;
echo rtrim(encode('test i')), PHP_EOL;
echo rtrim(decode('qtltbdxrxlk')), PHP_EOL;
