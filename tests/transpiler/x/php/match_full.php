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
function classify($n) {
  return match($n) {
    0 => "zero",
    1 => "one",
    default => "many",
};
}
echo rtrim((is_float(classify(0)) ? sprintf("%.15f", classify(0)) : classify(0))), PHP_EOL;
echo rtrim((is_float(classify(5)) ? sprintf("%.15f", classify(5)) : classify(5))), PHP_EOL;
?>
