<?php
function boom() {
    echo "boom", PHP_EOL;
    return true;
}
var_dump((1 < 2) && (2 < 3) && (3 < 4));
var_dump((1 < 2) && (2 > 3) && boom());
var_dump((1 < 2) && (2 < 3) && (3 > 4) && boom());
?>
