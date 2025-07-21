<?php
$xs = [1, 2, 3];
$ys = [];
foreach ($xs as $x) {
  if ($x % 2 == 1) {
    $ys[] = $x;
  }
}

echo (in_array(1, $ys) ? "True" : "False"), PHP_EOL;
echo (in_array(2, $ys) ? "True" : "False"), PHP_EOL;
$m = ["a" => 1];
echo (array_key_exists("a", $m) ? "True" : "False"), PHP_EOL;
echo (array_key_exists("b", $m) ? "True" : "False"), PHP_EOL;
$s = "hello";
echo (str_contains($s, "ell") ? "True" : "False"), PHP_EOL;
echo (str_contains($s, "foo") ? "True" : "False"), PHP_EOL;
