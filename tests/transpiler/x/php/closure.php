<?php
$makeAdder = function($n) {
  return function($x) use ($n) {
  return $x + $n;
};
};
$add10 = $makeAdder(10);
echo rtrim((is_float($add10(7)) ? sprintf("%.15f", $add10(7)) : $add10(7))), PHP_EOL;
?>
