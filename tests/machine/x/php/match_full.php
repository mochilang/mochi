<?php
$x = 2;
$label = match($x) {
    1 => "one",
    2 => "two",
    3 => "three",
    default => "unknown",
};
var_dump($label);
$day = "sun";
$mood = match($day) {
    "mon" => "tired",
    "fri" => "excited",
    "sun" => "relaxed",
    default => "normal",
};
var_dump($mood);
$ok = true;
$status = match($ok) {
    true => "confirmed",
    false => "denied",
    default => null,
};
var_dump($status);
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
