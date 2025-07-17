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
echo in_array(1, $ys), PHP_EOL;
echo in_array(2, $ys), PHP_EOL;
$m = ["a" => 1];
echo array_key_exists("a", $m), PHP_EOL;
echo array_key_exists("b", $m), PHP_EOL;
$s = "hello";
echo strpos($s, "ell") !== false, PHP_EOL;
echo strpos($s, "foo") !== false, PHP_EOL;
?>
