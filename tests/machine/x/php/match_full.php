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
    default => null,
};
echo $status, PHP_EOL;
function classify($n) {
    return match($n) {
    0 => "zero",
    1 => "one",
    default => "many",
};
}
var_dump(classify(0));
var_dump(classify(5));
?>
