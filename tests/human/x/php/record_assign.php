<?php
$c = ["n" => 0];
function inc(&$c) { $c['n'] = $c['n'] + 1; }
inc($c);
var_dump($c['n']);
?>
