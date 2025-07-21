<?php
$add = function($a, $b) {
  return $a + $b;
};
$add5 = $add(5);
echo rtrim((is_float($add5(3)) ? sprintf("%.15f", $add5(3)) : $add5(3))), PHP_EOL;
?>
