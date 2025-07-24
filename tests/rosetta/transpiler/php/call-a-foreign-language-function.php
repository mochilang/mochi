<?php
ini_set('memory_limit', '-1');
function strdup($s) {
  global $main;
  return $s . '';
}
function main() {
  global $strdup;
  $go1 = 'hello C';
  $c2 = strdup($go1);
  echo rtrim($c2), PHP_EOL;
}
main();
