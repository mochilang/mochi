<?php
$people = [
    ["name"=>"Alice","age"=>17,"status"=>"minor"],
    ["name"=>"Bob","age"=>25,"status"=>"unknown"],
    ["name"=>"Charlie","age"=>18,"status"=>"unknown"],
    ["name"=>"Diana","age"=>16,"status"=>"minor"],
];
foreach ($people as &$p){
    if($p['age']>=18){
        $p['status']="adult";
        $p['age']=$p['age']+1;
    }
}
$expected=[
    ["name"=>"Alice","age"=>17,"status"=>"minor"],
    ["name"=>"Bob","age"=>26,"status"=>"adult"],
    ["name"=>"Charlie","age"=>19,"status"=>"adult"],
    ["name"=>"Diana","age"=>16,"status"=>"minor"],
];
if($people==$expected){
    _print("ok");
}else{
    _print("mismatch");
}

function _print(...$args){
    $parts=[];
    foreach($args as $a){
        if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);} }
    echo implode(' ',$parts),PHP_EOL;
}
?>
