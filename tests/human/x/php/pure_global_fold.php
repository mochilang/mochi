<?php
$k = 2;
function inc($x) {
    global $k;
    return $x + $k;
}
var_dump(inc(3));
?>
