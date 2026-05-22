<?php
function outer($x) {
  $inner = function($y) use ($x) {
  return $x + $y;
};
  return $inner(5);
}
echo (is_float(outer(3)) ? json_encode(outer(3), 1344) : outer(3)), PHP_EOL;
