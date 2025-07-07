<?php
$data = [
    ["a"=>1,"b"=>2],
    ["a"=>1,"b"=>1],
    ["a"=>0,"b"=>5],
];
foreach ($data as $idx => &$d) { $d['_i']=$idx; }
usort($data, function($x,$y){
    if ($x['a'] == $y['a']) return ($x['b'] <=> $y['b']);
    return $x['a'] <=> $y['a'];
});
foreach ($data as &$d) { unset($d['_i']); }
$sorted = $data;
_print($sorted);

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
