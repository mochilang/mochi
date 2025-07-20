<?php
$boom = function() {
  echo "boom", PHP_EOL;
  return true;
};
echo ((1 < 2) && (2 < 3) && (3 < 4) ? 1 : 0), PHP_EOL;
echo ((1 < 2) && (2 > 3) && $boom() ? 1 : 0), PHP_EOL;
echo ((1 < 2) && (2 < 3) && (3 > 4) && $boom() ? 1 : 0), PHP_EOL;
?>
