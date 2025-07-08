<?php
function makeAdder($n) {
    return function($x) use ($n) {
        return $x + $n;
    };
}

$add10 = makeAdder(10);
var_dump($add10(7));
?>
