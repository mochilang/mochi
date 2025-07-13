<?php
$data = [
    ["tag" => "a", "val" => 1],
    ["tag" => "a", "val" => 2],
    ["tag" => "b", "val" => 3]
];
$groups = (function() use ($data) {
    $groups = [];
    foreach ($data as $d) {
        $_k = json_encode($d['tag']);
        $groups[$_k][] = $d;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = $g;
    }
    return $result;
})();
$tmp = [];
foreach ($groups as $g) {
    $total = 0;
    foreach ($g['items'] as $x) {
        $total = $total + $x['val'];
    }
    $tmp = array_merge($tmp, [["tag" => $g['key'], "total" => $total]]);
}
$result = (function() use ($tmp) {
    $result = [];
    foreach ($tmp as $r) {
        $result[] = [$r['tag'], $r];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
var_dump($result);
?>
