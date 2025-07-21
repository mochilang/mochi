<?php
$boom = function($a, $b) {
  echo rtrim("boom"), PHP_EOL;
  return true;
};
echo rtrim((is_float((false && $boom(1, 2) ? 1 : 0)) ? sprintf("%.15f", (false && $boom(1, 2) ? 1 : 0)) : (false && $boom(1, 2) ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((true || $boom(1, 2) ? 1 : 0)) ? sprintf("%.15f", (true || $boom(1, 2) ? 1 : 0)) : (true || $boom(1, 2) ? 1 : 0))), PHP_EOL;
?>
