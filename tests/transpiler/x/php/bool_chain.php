<?php
$boom = function() {
  echo rtrim("boom"), PHP_EOL;
  return true;
};
echo rtrim((is_float(((1 < 2) && (2 < 3) && (3 < 4) ? 1 : 0)) ? sprintf("%.15f", ((1 < 2) && (2 < 3) && (3 < 4) ? 1 : 0)) : ((1 < 2) && (2 < 3) && (3 < 4) ? 1 : 0))), PHP_EOL;
echo rtrim((is_float(((1 < 2) && (2 > 3) && $boom() ? 1 : 0)) ? sprintf("%.15f", ((1 < 2) && (2 > 3) && $boom() ? 1 : 0)) : ((1 < 2) && (2 > 3) && $boom() ? 1 : 0))), PHP_EOL;
echo rtrim((is_float(((1 < 2) && (2 < 3) && (3 > 4) && $boom() ? 1 : 0)) ? sprintf("%.15f", ((1 < 2) && (2 < 3) && (3 > 4) && $boom() ? 1 : 0)) : ((1 < 2) && (2 < 3) && (3 > 4) && $boom() ? 1 : 0))), PHP_EOL;
?>
