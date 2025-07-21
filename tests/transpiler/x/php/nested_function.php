<?php
$outer = function($x) {
  $inner = function($y) use ($x) {
  return $x + $y;
};
  return $inner(5);
};
echo rtrim($outer(3)), PHP_EOL;
?>
