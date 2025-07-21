<?php
$square = function($x) {
  return $x * $x;
};
echo rtrim((is_float($square(6)) ? sprintf("%.15f", $square(6)) : $square(6))), PHP_EOL;
?>
