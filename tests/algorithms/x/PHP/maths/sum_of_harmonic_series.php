<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function sum_of_harmonic_progression($first_term, $common_difference, $number_of_terms) {
  $arithmetic_progression = [1.0 / $first_term];
  $term = 1.0 / $first_term;
  $i = 0;
  while ($i < $number_of_terms - 1) {
  $term = $term + $common_difference;
  $arithmetic_progression = _append($arithmetic_progression, $term);
  $i = $i + 1;
};
  $total = 0.0;
  $j = 0;
  while ($j < count($arithmetic_progression)) {
  $total = $total + (1.0 / $arithmetic_progression[$j]);
  $j = $j + 1;
};
  return $total;
}
function abs_val($num) {
  if ($num < 0.0) {
  return -$num;
}
  return $num;
}
function test_sum_of_harmonic_progression() {
  $result1 = sum_of_harmonic_progression(0.5, 2.0, 2);
  if (abs_val($result1 - 0.75) > 0.0000001) {
  _panic('test1 failed');
}
  $result2 = sum_of_harmonic_progression(0.2, 5.0, 5);
  if (abs_val($result2 - 0.45666666666666667) > 0.0000001) {
  _panic('test2 failed');
}
}
function main() {
  test_sum_of_harmonic_progression();
  echo rtrim(json_encode(sum_of_harmonic_progression(0.5, 2.0, 2), 1344)), PHP_EOL;
}
main();
