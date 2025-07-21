<?php
$m = ["a" => 1, "b" => 2];
echo str_replace("    ", "  ", json_encode($m, 128)), PHP_EOL;
?>
