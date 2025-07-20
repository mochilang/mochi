<?php
function outer($x) {
  $inner = function($y) use ($x) {
  return $x + $y;
};
  return $inner(5);
}
echo outer(3), PHP_EOL;
?>
