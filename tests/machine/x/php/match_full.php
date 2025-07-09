<?php
$x = 2;
$label = match($x) {
    1 => "one",
    2 => "two",
    3 => "three",
    $_ => "unknown",
};
var_dump($label);
$day = "sun";
$mood = match($day) {
    "mon" => "tired",
    "fri" => "excited",
    "sun" => "relaxed",
    $_ => "normal",
};
var_dump($mood);
$ok = true;
$status = match($ok) {
    true => "confirmed",
    false => "denied",
};
var_dump($status);
function classify($n) {
    return match($n) {
    0 => "zero",
    1 => "one",
    $_ => "many",
};
}
var_dump(classify(0));
var_dump(classify(5));
