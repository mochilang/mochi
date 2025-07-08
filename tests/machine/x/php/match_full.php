<?php
/**
 * @param int $n
 * @return string
 */
function mochi_classify($n) {
	return (function($_t) {
	if ($_t === 0) return "zero";
	if ($_t === 1) return "one";
	return "many";
})($n);
}

// x: int
$x = 2;
// label: string
$label = (function($_t) {
	if ($_t === 1) return "one";
	if ($_t === 2) return "two";
	if ($_t === 3) return "three";
	return "unknown";
})($x);
_print($label);
// day: string
$day = "sun";
// mood: string
$mood = (function($_t) {
	if ($_t === "mon") return "tired";
	if ($_t === "fri") return "excited";
	if ($_t === "sun") return "relaxed";
	return "normal";
})($day);
_print($mood);
// ok: bool
$ok = true;
// status: string
$status = (function($_t) {
	if ($_t === true) return "confirmed";
	if ($_t === false) return "denied";
	return null;
})($ok);
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
