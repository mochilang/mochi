<?php
$inc = function($c) {
  $c["n"] = $c["n"] + 1;
};
$c = ["n" => 0];
$inc($c);
echo rtrim((is_float($c["n"]) ? sprintf("%.15f", $c["n"]) : $c["n"])), PHP_EOL;
?>
