<?php
$items=[['cat'=>'a','val'=>3],['cat'=>'a','val'=>1],['cat'=>'b','val'=>5],['cat'=>'b','val'=>2]];
$groups=[];
foreach($items as $i){
    if(!isset($groups[$i['cat']]))$groups[$i['cat']]=0;
    $groups[$i['cat']]+=$i['val'];
}
$grouped=[];
foreach($groups as $cat=>$total){$grouped[]=['cat'=>$cat,'total'=>$total];}
usort($grouped,function($a,$b){return $b['total']<=>$a['total'];});
_print($grouped);

function _print(...$args){$parts=[];foreach($args as $a){if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);}}echo implode(' ',$parts),PHP_EOL;}
?>
