<?php
$nation=[["n_nationkey"=>1,"n_name"=>"BRAZIL"]];
$customer=[["c_custkey"=>1,"c_name"=>"Alice","c_acctbal"=>100.0,"c_nationkey"=>1,"c_address"=>"123 St","c_phone"=>"123-456","c_comment"=>"Loyal"]];
$orders=[["o_orderkey"=>1000,"o_custkey"=>1,"o_orderdate"=>"1993-10-15"],["o_orderkey"=>2000,"o_custkey"=>1,"o_orderdate"=>"1994-01-02"]];
$lineitem=[["l_orderkey"=>1000,"l_returnflag"=>"R","l_extendedprice"=>1000.0,"l_discount"=>0.1],["l_orderkey"=>2000,"l_returnflag"=>"N","l_extendedprice"=>500.0,"l_discount"=>0.0]];
$start_date="1993-10-01";$end_date="1994-01-01";
$map=[];
foreach($customer as $c){
  foreach($orders as $o){
    if($o['o_custkey']==$c['c_custkey'] && $o['o_orderdate']>=$start_date && $o['o_orderdate']<$end_date){
      foreach($lineitem as $l){
        if($l['l_orderkey']==$o['o_orderkey'] && $l['l_returnflag']=="R"){
          foreach($nation as $n){
            if($n['n_nationkey']==$c['c_nationkey']){
              $key=json_encode(['c_custkey'=>$c['c_custkey'],'c_name'=>$c['c_name'],'c_acctbal'=>$c['c_acctbal'],'c_address'=>$c['c_address'],'c_phone'=>$c['c_phone'],'c_comment'=>$c['c_comment'],'n_name'=>$n['n_name']]);
              $val=$l['l_extendedprice']*(1-$l['l_discount']);
              if(!isset($map[$key]))$map[$key]=0;
              $map[$key]+=$val;
            }
          }
        }
      }
    }
  }
}
$result=[];
foreach($map as $k=>$rev){$key=json_decode($k,true);$result[]=[
  'c_custkey'=>$key['c_custkey'],
  'c_name'=>$key['c_name'],
  'revenue'=>$rev,
  'c_acctbal'=>$key['c_acctbal'],
  'n_name'=>$key['n_name'],
  'c_address'=>$key['c_address'],
  'c_phone'=>$key['c_phone'],
  'c_comment'=>$key['c_comment']];}
usort($result,function($a,$b){return $b['revenue']<=>$a['revenue'];});
var_dump($result);

function var_dump(...$args){$parts=[];foreach($args as $a){if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);}}echo implode(' ',$parts),PHP_EOL;}
?>
