<?php
$k = 2;
$inc = function($x) use ($k) {
  return $x + $k;
};
echo rtrim($inc(3)), PHP_EOL;
?>
