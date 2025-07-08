<?php
$data=[['tag'=>'a','val'=>1],['tag'=>'a','val'=>2],['tag'=>'b','val'=>3]];
$groups=[];
foreach($data as $d){$groups[$d['tag']][]=$d;}
$tmp=[];
foreach($groups as $tag=>$items){$total=0;foreach($items as $x){$total+=$x['val'];}$tmp[]=['tag'=>$tag,'total'=>$total];}
usort($tmp,function($a,$b){return $a['tag']<=>$b['tag'];});
$result=$tmp;
var_dump($result);

function var_dump(...$args){$parts=[];foreach($args as $a){if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);}}echo implode(' ',$parts),PHP_EOL;}
?>
