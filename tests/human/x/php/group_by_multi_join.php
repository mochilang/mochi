<?php
$nations=[["id"=>1,"name"=>"A"],["id"=>2,"name"=>"B"]];
$suppliers=[["id"=>1,"nation"=>1],["id"=>2,"nation"=>2]];
$partsupp=[['part'=>100,'supplier'=>1,'cost'=>10.0,'qty'=>2],['part'=>100,'supplier'=>2,'cost'=>20.0,'qty'=>1],['part'=>200,'supplier'=>1,'cost'=>5.0,'qty'=>3]];
$filtered=[];
foreach($partsupp as $ps){
  foreach($suppliers as $s){
    if($s['id']==$ps['supplier']){
      foreach($nations as $n){
        if($n['id']==$s['nation'] && $n['name']=='A'){
          $filtered[]=['part'=>$ps['part'],'value'=>$ps['cost']*$ps['qty']];
        }
      }
    }
  }
}
$groups=[];
foreach($filtered as $x){
  if(!isset($groups[$x['part']]))$groups[$x['part']]=0;
  $groups[$x['part']]+=$x['value'];
}
$grouped=[];
foreach($groups as $p=>$tot){$grouped[]=['part'=>$p,'total'=>$tot];}
_print($grouped);

function _print(...$args){$parts=[];foreach($args as $a){if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);}}echo implode(' ',$parts),PHP_EOL;}
?>
