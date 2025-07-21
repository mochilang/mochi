<?php
function add($a, $b) {
  return $a + $b;
}
$add5 = function($a0) {
  return add(5, $a0);
};
echo (is_float($add5(3)) ? json_encode($add5(3), 1344) : $add5(3)), PHP_EOL;
