<?php
$x = 2;
$label = match($x) {
    1 => "one",
    2 => "two",
    3 => "three",
    default => "unknown",
};
_print($label);

$day = "sun";
$mood = match($day) {
    "mon" => "tired",
    "fri" => "excited",
    "sun" => "relaxed",
    default => "normal",
};
_print($mood);

$ok = true;
$status = $ok ? "confirmed" : "denied";
_print($status);

function classify($n) {
    return match($n) {
        0 => "zero",
        1 => "one",
        default => "many",
    };
}
_print(classify(0));
_print(classify(5));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
?>
