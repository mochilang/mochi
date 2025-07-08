<?php
function outer($x) {
    $inner = function($y) use ($x) {
        return $x + $y;
    };
    return $inner(5);
}
var_dump(outer(3));
?>
