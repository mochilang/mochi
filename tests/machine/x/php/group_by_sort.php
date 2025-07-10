<?php
$items = [["cat" => "a", "val" => 3], ["cat" => "a", "val" => 1], ["cat" => "b", "val" => 5], ["cat" => "b", "val" => 2]];
$grouped = (function() use ($items) {
    $groups = [];
    foreach ($items as $i) {
        $_k = json_encode($i['cat']);
        $groups[$_k][] = $i;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        $result[] = [-array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['val'];
    }
    return $result;
})()), ["cat" => $g['key'], "total" => array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['val'];
    }
    return $result;
})())]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
var_dump($grouped);
?>
