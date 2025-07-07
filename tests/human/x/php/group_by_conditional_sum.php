<?php
$items = [
    ["cat" => "a", "val" => 10, "flag" => true],
    ["cat" => "a", "val" => 5, "flag" => false],
    ["cat" => "b", "val" => 20, "flag" => true],
];
$groups = [];
foreach ($items as $it) {
    $groups[$it['cat']][] = $it;
}
ksort($groups);
$result = [];
foreach ($groups as $cat => $group) {
    $total = 0;
    $flagged = 0;
    foreach ($group as $x) {
        $total += $x['val'];
        if ($x['flag']) {
            $flagged += $x['val'];
        }
    }
    $result[] = ["cat" => $cat, "share" => $flagged / $total];
}
_print($result);

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
?>
