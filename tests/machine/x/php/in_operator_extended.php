<?php
$xs = [1, 2, 3];
$ys = (function() use ($xs) {
    $result = [];
    foreach ($xs as $x) {
        if ($x % 2 == 1) {
            $result[] = $x;
        }
    }
    return $result;
})();
_print(in_array(1, $ys));
_print(in_array(2, $ys));
$m = ["a" => 1];
_print(array_key_exists("a", $m));
_print(array_key_exists("b", $m));
$s = "hello";
_print(strpos($s, "ell") !== false);
_print(strpos($s, "foo") !== false);
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
