<?php
ini_set('memory_limit', '-1');
function bellTriangle($n) {
  global $main;
  $tri = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $i) {
  $row = array_merge($row, [0]);
  $j = $j + 1;
};
  $tri = array_merge($tri, [$row]);
  $i = $i + 1;
};
  $tri[1][0] = 1;
  $i = 2;
  while ($i < $n) {
  $tri[$i][0] = $tri[$i - 1][$i - 2];
  $j = 1;
  while ($j < $i) {
  $tri[$i][$j] = $tri[$i][$j - 1] + $tri[$i - 1][$j - 1];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $tri;
}
function main() {
  global $bellTriangle;
  $bt = bellTriangle(51);
  echo rtrim('First fifteen and fiftieth Bell numbers:'), PHP_EOL;
  for ($i = 1; $i < 16; $i++) {
  echo rtrim('' . str_pad(json_encode($i, 1344), 2, ' ', STR_PAD_LEFT) . ': ' . json_encode($bt[$i][0], 1344)), PHP_EOL;
};
  echo rtrim('50: ' . json_encode($bt[50][0], 1344)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim('The first ten rows of Bell\'s triangle:'), PHP_EOL;
  for ($i = 1; $i < 11; $i++) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($bt[$i], 1344))))))), PHP_EOL;
};
}
main();
