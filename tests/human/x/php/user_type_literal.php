<?php
$book = [
    'title' => 'Go',
    'author' => ['name'=>'Bob','age'=>42],
];
_print($book['author']['name']);

function _print(...$args){
    $parts=[];
    foreach($args as $a){
        if(is_array($a)||is_object($a)){$parts[]=json_encode($a);}else{$parts[]=strval($a);} }
    echo implode(' ',$parts),PHP_EOL;
}
?>
