<?php
function inc($c) {
  $c["n"] = $c["n"] + 1;
}
$c = ["n" => 0];
inc($c);
echo (is_float($c["n"]) ? json_encode($c["n"], 1344) : $c["n"]), PHP_EOL;
