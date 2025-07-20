<?php
function boom($a, $b) {
  echo "boom", PHP_EOL;
  return true;
}
echo (false && boom(1, 2) ? 1 : 0), PHP_EOL;
echo (true || boom(1, 2) ? 1 : 0), PHP_EOL;
?>
