<?php
$xs = [1, 2, 3];
$ys = (function() {
    $result = [];
    foreach ($xs as $x) {
        if ($x % 2 == 1) {
            $result[] = $x;
        }
    }
    return $result;
})();
var_dump(in_array(1, $ys));
var_dump(in_array(2, $ys));
$m = ["a" => 1];
var_dump(array_key_exists("a", $m));
var_dump(array_key_exists("b", $m));
$s = "hello";
var_dump(strpos($s, "ell") !== false);
var_dump(strpos($s, "foo") !== false);
