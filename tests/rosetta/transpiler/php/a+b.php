<?php
function main() {
  $a = intval(trim(fgets(STDIN)));
  $b = intval(trim(fgets(STDIN)));
  echo json_encode($a + $b, 1344), PHP_EOL;
}
main();
