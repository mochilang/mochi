<?php
$k = 2;
function inc($x) {
  return $x + $k;
}
echo rtrim((is_float(inc(3)) ? sprintf("%.15f", inc(3)) : inc(3))), PHP_EOL;
?>
