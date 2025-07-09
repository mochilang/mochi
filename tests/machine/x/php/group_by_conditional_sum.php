<?php
$items = [["cat" => "a", "val" => 10, "flag" => true], ["cat" => "a", "val" => 5, "flag" => false], ["cat" => "b", "val" => 20, "flag" => true]];
$result = (function() {
    $groups = [];
    foreach ($items as $i) {
        $_k = json_encode($i->cat);
        $groups[$_k][] = $i;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        $result[] = [$g->key, ["cat" => $g->key, "share" => array_sum((function() {
    $result = [];
    foreach ($g as $x) {
        $result[] = ($x->flag ? $x->val : 0);
    }
    return $result;
})()) / array_sum((function() {
    $result = [];
    foreach ($g as $x) {
        $result[] = $x->val;
    }
    return $result;
})())]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
var_dump($result);
