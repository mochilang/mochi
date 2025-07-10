<?php
$data = [["a" => 1, "b" => 2], ["a" => 1, "b" => 1], ["a" => 0, "b" => 5]];
$sorted = (function() use ($data) {
    $result = [];
    foreach ($data as $x) {
        $result[] = [["a" => $x->a, "b" => $x->b], $x];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
var_dump($sorted);
