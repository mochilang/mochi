<?php
$xs = [1, 2, 3];
$ys = array_values(array_filter($xs, fn($x) => $x % 2 == 1));
_print(in_array(1, $ys));
_print(in_array(2, $ys));

$m = ["a" => 1];
_print(array_key_exists("a", $m));
_print(array_key_exists("b", $m));

$s = "hello";
_print(strpos($s, "ell") !== false);
_print(strpos($s, "foo") !== false);

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
