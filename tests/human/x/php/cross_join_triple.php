<?php
$nums = [1,2];
$letters = ["A","B"];
$bools = [true,false];
$combos = [];
foreach ($nums as $n) {
    foreach ($letters as $l) {
        foreach ($bools as $b) {
            $combos[] = ["n"=>$n,"l"=>$l,"b"=>$b];
        }
    }
}
var_dump("--- Cross Join of three lists ---");
foreach ($combos as $c) {
    var_dump($c['n'], $c['l'], $c['b'] ? 'true' : 'false');
}
?>
