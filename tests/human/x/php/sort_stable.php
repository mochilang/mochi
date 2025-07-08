<?php
$items = [
    ["n"=>1,"v"=>"a"],
    ["n"=>1,"v"=>"b"],
    ["n"=>2,"v"=>"c"],
];
foreach ($items as $i=>&$it){$it['_i']=$i;}
usort($items,function($x,$y){
    if($x['n']==$y['n']) return $x['_i'] <=> $y['_i'];
    return $x['n'] <=> $y['n'];
});
$result = array_map(fn($x)=>$x['v'],$items);
var_dump($result);
?>
