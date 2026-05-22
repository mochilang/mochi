<?php
function makeAdder($n) {
  return function($x) use ($n) {
  return $x + $n;
};
}
$add10 = makeAdder(10);
echo (is_float($add10(7)) ? json_encode($add10(7), 1344) : $add10(7)), PHP_EOL;
