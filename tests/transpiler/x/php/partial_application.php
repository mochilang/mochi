<?php
function add($a, $b) {
  return $a + $b;
}
$add5 = function($a0) {
  return add(5, $a0);
};
echo rtrim((is_float($add5(3)) ? sprintf("%.15f", $add5(3)) : $add5(3))), PHP_EOL;
?>
