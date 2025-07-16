<?php
$nums = [1, 2, 3];
$letters = ["A", "B"];
$pairs = (function() use ($letters, $nums) {
    $result = [];
    foreach ($nums as $n) {
        foreach ($letters as $l) {
            if ($n % 2 == 0) {
                $result[] = ["n" => $n, "l" => $l];
            }
        }
    }
    return $result;
})();
_print("--- Even pairs ---");
foreach ($pairs as $p) {
    _print($p['n'], $p['l']);
}
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
