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
var_dump($result);
?>
