<?php
$items = [[$n => 1, $v => "a"], [$n => 1, $v => "b"], [$n => 2, $v => "c"]];
$result = (function() {
    $result = [];
    foreach ($items as $i) {
        $result[] = [$i->n, $i->v];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();;
var_dump($result);
