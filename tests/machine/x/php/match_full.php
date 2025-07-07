<?php
/**
 * @param int $n
 * @return string
 */
function mochi_classify($n) {
	return match ($n) { 0 => "zero", 1 => "one", default => "many" };
}

// x: int
$x = 2;
// label: string
$label = match ($x) { 1 => "one", 2 => "two", 3 => "three", default => "unknown" };
_print($label);
// day: string
$day = "sun";
// mood: string
$mood = match ($day) { "mon" => "tired", "fri" => "excited", "sun" => "relaxed", default => "normal" };
_print($mood);
// ok: bool
$ok = true;
// status: string
$status = match ($ok) { true => "confirmed", false => "denied" };
_print($status);
_print(mochi_classify(0));
_print(mochi_classify(5));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
