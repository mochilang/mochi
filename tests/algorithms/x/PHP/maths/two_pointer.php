<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function two_pointer($nums, $target) {
  $i = 0;
  $j = count($nums) - 1;
  while ($i < $j) {
  $s = $nums[$i] + $nums[$j];
  if ($s == $target) {
  return [$i, $j];
}
  if ($s < $target) {
  $i = $i + 1;
} else {
  $j = $j - 1;
}
};
  return [];
}
function test_two_pointer() {
  if (two_pointer([2, 7, 11, 15], 9) != [0, 1]) {
  _panic('case1');
}
  if (two_pointer([2, 7, 11, 15], 17) != [0, 3]) {
  _panic('case2');
}
  if (two_pointer([2, 7, 11, 15], 18) != [1, 2]) {
  _panic('case3');
}
  if (two_pointer([2, 7, 11, 15], 26) != [2, 3]) {
  _panic('case4');
}
  if (two_pointer([1, 3, 3], 6) != [1, 2]) {
  _panic('case5');
}
  if (count(two_pointer([2, 7, 11, 15], 8)) != 0) {
  _panic('case6');
}
  if (count(two_pointer([0, 3, 6, 9, 12, 15, 18, 21, 24, 27], 19)) != 0) {
  _panic('case7');
}
  if (count(two_pointer([1, 2, 3], 6)) != 0) {
  _panic('case8');
}
}
function main() {
  test_two_pointer();
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(two_pointer([2, 7, 11, 15], 9), 1344)))))), PHP_EOL;
}
main();
