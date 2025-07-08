<?php
function expect($cond){
    if(!$cond){
        echo "Expectation failed".PHP_EOL;
    }
}
$x = 1 + 2;
expect($x == 3);
var_dump("ok");
?>
