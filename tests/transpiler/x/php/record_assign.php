<?php
$inc = function($c) {
  $c["n"] = $c["n"] + 1;
};
$c = ["n" => 0];
$inc($c);
echo rtrim($c["n"]), PHP_EOL;
?>
