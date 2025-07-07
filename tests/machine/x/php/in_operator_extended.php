<?php
// xs: [int]
$xs = [1, 2, 3];
// ys: [int]
$ys = (function() use ($xs) {
	$res = [];
	foreach ((is_string($xs) ? str_split($xs) : $xs) as $x) {
		if (!((((is_int($x) && is_int(2)) ? ($x % 2) : fmod($x, 2)) == 1))) { continue; }
		$res[] = $x;
	}
	return $res;
})();
_print((is_array($ys) ? (array_key_exists(1, $ys) || in_array(1, $ys, true)) : (is_string($ys) ? strpos($ys, strval(1)) !== false : false)));
_print((is_array($ys) ? (array_key_exists(2, $ys) || in_array(2, $ys, true)) : (is_string($ys) ? strpos($ys, strval(2)) !== false : false)));
// m: {string: int}
$m = ["a" => 1];
_print((is_array($m) ? (array_key_exists("a", $m) || in_array("a", $m, true)) : (is_string($m) ? strpos($m, strval("a")) !== false : false)));
_print((is_array($m) ? (array_key_exists("b", $m) || in_array("b", $m, true)) : (is_string($m) ? strpos($m, strval("b")) !== false : false)));
// s: string
$s = "hello";
_print((is_array($s) ? (array_key_exists("ell", $s) || in_array("ell", $s, true)) : (is_string($s) ? strpos($s, strval("ell")) !== false : false)));
_print((is_array($s) ? (array_key_exists("foo", $s) || in_array("foo", $s, true)) : (is_string($s) ? strpos($s, strval("foo")) !== false : false)));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
