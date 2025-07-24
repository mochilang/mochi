<?php
ini_set('memory_limit', '-1');
function parseBool($s) {
  global $main;
  $l = strtolower($s);
  if ($l == '1' || $l == 't' || $l == true || $l == 'yes' || $l == 'y') {
  return true;
}
  return false;
}
function main() {
  global $parseBool;
  $n = true;
  echo rtrim(($n ? 'true' : 'false')), PHP_EOL;
  echo rtrim('bool'), PHP_EOL;
  $n = !$n;
  echo rtrim(($n ? 'true' : 'false')), PHP_EOL;
  $x = 5;
  $y = 8;
  echo rtrim('x == y:') . " " . rtrim(($x == $y ? 'true' : 'false')), PHP_EOL;
  echo rtrim('x < y:') . " " . rtrim(($x < $y ? 'true' : 'false')), PHP_EOL;
  echo rtrim('
Convert String into Boolean Data type
'), PHP_EOL;
  $str1 = 'japan';
  echo rtrim('Before :') . " " . rtrim('string'), PHP_EOL;
  $bolStr = parseBool($str1);
  echo rtrim('After :') . " " . rtrim('bool'), PHP_EOL;
}
main();
