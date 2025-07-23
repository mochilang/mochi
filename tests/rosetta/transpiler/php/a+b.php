<?php
ini_set('memory_limit','-1');
function main() {
  $a = intval(trim(fgets(STDIN)));
  $b = intval(trim(fgets(STDIN)));
  echo json_encode($a + $b, 1344), PHP_EOL;
}
main();
