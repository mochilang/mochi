<?php
$x = 2;
$label = match($x) {
    1 => "one",
    2 => "two",
    3 => "three",
    default => "unknown",
};
echo $label, PHP_EOL;
$day = "sun";
$mood = match($day) {
    "mon" => "tired",
    "fri" => "excited",
    "sun" => "relaxed",
    default => "normal",
};
echo $mood, PHP_EOL;
$ok = true;
$status = match($ok) {
    true => "confirmed",
    false => "denied",
};
echo $status, PHP_EOL;
function classify($n) {
  return match($n) {
    0 => "zero",
    1 => "one",
    default => "many",
};
}
echo (is_float(classify(0)) ? json_encode(classify(0), 1344) : classify(0)), PHP_EOL;
echo (is_float(classify(5)) ? json_encode(classify(5), 1344) : classify(5)), PHP_EOL;
