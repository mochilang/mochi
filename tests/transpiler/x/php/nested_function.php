<?php
function outer($x) {
  $inner = function($y) use ($x) {
  return $x + $y;
};
  return $inner(5);
}
echo rtrim((is_float(outer(3)) ? sprintf("%.15f", outer(3)) : outer(3))), PHP_EOL;
?>
