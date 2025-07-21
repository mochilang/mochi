<?php
$x = 2;
$label = match($x) {
    1 => "one",
    2 => "two",
    3 => "three",
    default => "unknown",
};
echo rtrim($label), PHP_EOL;
$day = "sun";
$mood = match($day) {
    "mon" => "tired",
    "fri" => "excited",
    "sun" => "relaxed",
    default => "normal",
};
echo rtrim($mood), PHP_EOL;
$ok = true;
$status = match($ok) {
    true => "confirmed",
    false => "denied",
};
echo rtrim($status), PHP_EOL;
$classify = function($n) use ($x, $label, $day, $mood, $ok, $status) {
  return match($n) {
    0 => "zero",
    1 => "one",
    default => "many",
};
};
echo rtrim($classify(0)), PHP_EOL;
echo rtrim($classify(5)), PHP_EOL;
?>
