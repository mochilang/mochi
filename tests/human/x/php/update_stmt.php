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
    var_dump("ok");
}else{
    var_dump("mismatch");
}
?>
