<?php
$xs = [1, 2, 3];
$ys = [];
foreach ($xs as $x) {
  if ($x % 2 == 1) {
    $ys[] = $x;
  }
}

echo rtrim((is_float((in_array(1, $ys) ? 1 : 0)) ? sprintf("%.15f", (in_array(1, $ys) ? 1 : 0)) : (in_array(1, $ys) ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((in_array(2, $ys) ? 1 : 0)) ? sprintf("%.15f", (in_array(2, $ys) ? 1 : 0)) : (in_array(2, $ys) ? 1 : 0))), PHP_EOL;
$m = ["a" => 1];
echo rtrim((is_float((array_key_exists("a", $m) ? 1 : 0)) ? sprintf("%.15f", (array_key_exists("a", $m) ? 1 : 0)) : (array_key_exists("a", $m) ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((array_key_exists("b", $m) ? 1 : 0)) ? sprintf("%.15f", (array_key_exists("b", $m) ? 1 : 0)) : (array_key_exists("b", $m) ? 1 : 0))), PHP_EOL;
$s = "hello";
echo rtrim((is_float((str_contains($s, "ell") ? 1 : 0)) ? sprintf("%.15f", (str_contains($s, "ell") ? 1 : 0)) : (str_contains($s, "ell") ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((str_contains($s, "foo") ? 1 : 0)) ? sprintf("%.15f", (str_contains($s, "foo") ? 1 : 0)) : (str_contains($s, "foo") ? 1 : 0))), PHP_EOL;
?>
