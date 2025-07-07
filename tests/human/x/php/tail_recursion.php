<?php
function sum_rec($n,$acc){
    if($n==0) return $acc;
    return sum_rec($n-1,$acc+$n);
}
_print(sum_rec(10,0));

function _print(...$args){
    $parts=[];
    foreach($args as $a){
        if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);} }
    echo implode(' ',$parts),PHP_EOL;
}
?>
