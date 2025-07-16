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
_print($result);
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
