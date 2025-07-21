<?php
function boom() {
  echo rtrim("boom"), PHP_EOL;
  return true;
}
echo rtrim(((1 < 2) && (2 < 3) && (3 < 4) ? "true" : "false")), PHP_EOL;
echo rtrim(((1 < 2) && (2 > 3) && boom() ? "true" : "false")), PHP_EOL;
echo rtrim(((1 < 2) && (2 < 3) && (3 > 4) && boom() ? "true" : "false")), PHP_EOL;
?>
