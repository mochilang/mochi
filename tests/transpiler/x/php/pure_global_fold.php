<?php
$k = 2;
$inc = function($x) use ($k) {
  return $x + $k;
};
echo $inc(3), PHP_EOL;
?>
