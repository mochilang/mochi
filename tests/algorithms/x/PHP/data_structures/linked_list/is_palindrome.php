<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function is_palindrome($values) {
  $stack = [];
  $fast = 0;
  $slow = 0;
  $n = count($values);
  while ($fast < $n && $fast + 1 < $n) {
  $stack = _append($stack, $values[$slow]);
  $slow = $slow + 1;
  $fast = $fast + 2;
};
  if ($fast == $n - 1) {
  $slow = $slow + 1;
}
  $i = count($stack) - 1;
  while ($slow < $n) {
  if ($stack[$i] != $values[$slow]) {
  return false;
}
  $i = $i - 1;
  $slow = $slow + 1;
};
  return true;
}
function main() {
  echo rtrim(json_encode(is_palindrome([]), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_palindrome([1]), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_palindrome([1, 2]), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_palindrome([1, 2, 1]), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_palindrome([1, 2, 2, 1]), 1344)), PHP_EOL;
}
main();
