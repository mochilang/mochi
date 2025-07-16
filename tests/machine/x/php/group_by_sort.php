<?php
$items = [
    ["cat" => "a", "val" => 3],
    ["cat" => "a", "val" => 1],
    ["cat" => "b", "val" => 5],
    ["cat" => "b", "val" => 2]
];
$grouped = (function() use ($items) {
    $groups = [];
    foreach ($items as $i) {
        $_k = json_encode($i['cat']);
        $groups[$_k][] = $i;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [-array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['val'];
    }
    return $result;
})()), [
    "cat" => $g['key'],
    "total" => array_sum((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $x) {
            $result[] = $x['val'];
        }
        return $result;
    })())
]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
_print($grouped);
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
