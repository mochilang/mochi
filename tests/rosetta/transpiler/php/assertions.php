<?php
function main() {
  $x = 43;
  if ($x != 42) {
  echo "Assertion failed", PHP_EOL;
} else {
  echo "Assertion passed", PHP_EOL;
}
}
main();
