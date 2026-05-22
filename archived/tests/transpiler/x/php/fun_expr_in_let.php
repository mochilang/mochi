<?php
$square = function($x) {
  return $x * $x;
};
echo (is_float($square(6)) ? json_encode($square(6), 1344) : $square(6)), PHP_EOL;
