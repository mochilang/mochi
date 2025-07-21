<?php
$k = 2;
function inc($x) {
  return $x + $k;
}
echo (is_float(inc(3)) ? json_encode(inc(3), 1344) : inc(3)), PHP_EOL;
