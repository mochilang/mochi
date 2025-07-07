<?php
function expect($cond){
    if(!$cond){
        echo "Expectation failed".PHP_EOL;
    }
}
$x = 1 + 2;
expect($x == 3);
_print("ok");

function _print(...$args){
    $parts=[];
    foreach($args as $a){
        if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);} }
    echo implode(' ',$parts),PHP_EOL;
}
?>
