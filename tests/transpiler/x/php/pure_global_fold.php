<?php
$k = 2;
$inc = function($x) use ($k) {
  return $x + $k;
};
echo rtrim((is_float($inc(3)) ? sprintf("%.15f", $inc(3)) : $inc(3))), PHP_EOL;
?>
