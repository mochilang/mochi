<?php
function twoSum($nums,$target){
    $n=count($nums);
    for($i=0;$i<$n;$i++){
        for($j=$i+1;$j<$n;$j++){
            if($nums[$i]+$nums[$j]==$target){
                return [$i,$j];
            }
        }
    }
    return [-1,-1];
}
$result=twoSum([2,7,11,15],9);
_print($result[0]);
_print($result[1]);

function _print(...$args){
    $parts=[];
    foreach($args as $a){
        if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);} }
    echo implode(' ',$parts),PHP_EOL;
}
?>
