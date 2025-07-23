<?php
ini_set('memory_limit','-1');
function bottles($n) {
  global $main;
  if ($n == 0) {
  return "No more bottles";
}
  if ($n == 1) {
  return "1 bottle";
}
  return json_encode($n, 1344) . " bottles";
}
function main() {
  global $bottles;
  $i = 99;
  while ($i > 0) {
  echo bottles($i) . " of beer on the wall", PHP_EOL;
  echo bottles($i) . " of beer", PHP_EOL;
  echo "Take one down, pass it around", PHP_EOL;
  echo bottles($i - 1) . " of beer on the wall", PHP_EOL;
  $i = $i - 1;
};
}
main();
