<?php
function boom($a, $b) {
    var_dump("boom");
    return true;
}
var_dump(false && boom(1, 2));
var_dump(true || boom(1, 2));
