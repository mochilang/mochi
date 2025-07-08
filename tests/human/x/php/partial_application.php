<?php
function add($a, $b) {
    return $a + $b;
}
$add5 = function($b) { return add(5, $b); };
var_dump($add5(3));
?>
