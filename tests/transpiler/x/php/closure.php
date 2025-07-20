<?php
$makeAdder = function($n) {
  return function($x) use ($n) {
  return $x + $n;
};
};
$add10 = $makeAdder(10);
echo $add10(7), PHP_EOL;
?>
