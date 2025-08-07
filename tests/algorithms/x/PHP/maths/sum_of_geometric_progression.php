<?php
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function pow_float($base, $exp) {
  $result = 1.0;
  $exponent = $exp;
  if ($exponent < 0) {
  $exponent = -$exponent;
  $i = 0;
  while ($i < $exponent) {
  $result = $result * $base;
  $i = $i + 1;
};
  return 1.0 / $result;
}
  $i = 0;
  while ($i < $exponent) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function sum_of_geometric_progression($first_term, $common_ratio, $num_of_terms) {
  if ($common_ratio == 1) {
  return floatval(($num_of_terms * $first_term));
}
  $a = floatval($first_term);
  $r = floatval($common_ratio);
  return ($a / (1.0 - $r)) * (1.0 - pow_float($r, $num_of_terms));
}
function test_sum() {
  if (sum_of_geometric_progression(1, 2, 10) != 1023.0) {
  _panic('example1 failed');
}
  if (sum_of_geometric_progression(1, 10, 5) != 11111.0) {
  _panic('example2 failed');
}
  if (sum_of_geometric_progression(-1, 2, 10) != (-1023.0)) {
  _panic('example3 failed');
}
}
function main() {
  test_sum();
  echo rtrim(json_encode(sum_of_geometric_progression(1, 2, 10), 1344)), PHP_EOL;
}
main();
