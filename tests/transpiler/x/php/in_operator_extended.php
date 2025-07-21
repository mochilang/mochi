<?php
$xs = [1, 2, 3];
$ys = [];
foreach ($xs as $x) {
  if ($x % 2 == 1) {
    $ys[] = $x;
  }
}

echo rtrim((in_array(1, $ys) ? "true" : "false")), PHP_EOL;
echo rtrim((in_array(2, $ys) ? "true" : "false")), PHP_EOL;
$m = ["a" => 1];
echo rtrim((array_key_exists("a", $m) ? "true" : "false")), PHP_EOL;
echo rtrim((array_key_exists("b", $m) ? "true" : "false")), PHP_EOL;
$s = "hello";
echo rtrim((str_contains($s, "ell") ? "true" : "false")), PHP_EOL;
echo rtrim((str_contains($s, "foo") ? "true" : "false")), PHP_EOL;
?>
