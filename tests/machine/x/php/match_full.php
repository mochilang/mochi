<?php
$x = 2;
$label = (function($_t) {
    if ($_t === 1) return "one";
    if ($_t === 2) return "two";
    if ($_t === 3) return "three";
    return "unknown";
})($x);
var_dump($label);
$day = "sun";
$mood = (function($_t) {
    if ($_t === "mon") return "tired";
    if ($_t === "fri") return "excited";
    if ($_t === "sun") return "relaxed";
    return "normal";
})($day);
var_dump($mood);
$ok = true;
$status = (function($_t) {
    if ($_t === true) return "confirmed";
    if ($_t === false) return "denied";
    return null;
})($ok);
var_dump($status);
function classify($n) {
    return (function($_t) {
    if ($_t === 0) return "zero";
    if ($_t === 1) return "one";
    return "many";
})($n);
}
var_dump(classify(0));
var_dump(classify(5));
