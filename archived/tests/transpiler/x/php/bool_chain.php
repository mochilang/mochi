<?php
function boom() {
  echo "boom", PHP_EOL;
  return true;
}
echo ((1 < 2) && (2 < 3) && (3 < 4) ? "True" : "False"), PHP_EOL;
echo ((1 < 2) && (2 > 3) && boom() ? "True" : "False"), PHP_EOL;
echo ((1 < 2) && (2 < 3) && (3 > 4) && boom() ? "True" : "False"), PHP_EOL;
