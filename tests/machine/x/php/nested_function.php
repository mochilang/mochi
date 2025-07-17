<?php
function outer($x) {
    function inner($y) {
        return $x + $y;
    }
    return inner(5);
}
var_dump(outer(3));
?>
