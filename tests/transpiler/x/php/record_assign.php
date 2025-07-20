<?php
$inc = function($c) {
  $c["n"] = $c["n"] + 1;
};
$c = ["n" => 0];
$inc($c);
echo $c["n"], PHP_EOL;
?>
