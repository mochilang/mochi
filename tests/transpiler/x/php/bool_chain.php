<?php
function boom() {
  echo "boom", PHP_EOL;
  return true;
}
echo ((1 < 2) && (2 < 3) && (3 < 4) ? "true" : "false"), PHP_EOL;
echo ((1 < 2) && (2 > 3) && boom() ? "true" : "false"), PHP_EOL;
echo ((1 < 2) && (2 < 3) && (3 > 4) && boom() ? "true" : "false"), PHP_EOL;
?>