<?php
$math = [
    'sqrt' => function($x) { return sqrt($x); },
    'pow' => function($x, $y) { return pow($x, $y); },
    'sin' => function($x) { return sin($x); },
    'log' => function($x) { return log($x); },
    'pi' => M_PI,
    'e' => M_E,
];
$r = 3;
$area = $math['pi'] * $math['pow']($r, 2);
$root = $math['sqrt'](49);
$sin45 = $math['sin']($math['pi'] / 4);
$log_e = $math['log']($math['e']);
var_dump("Circle area with r =", $r, "=>", $area);
var_dump("Square root of 49:", $root);
var_dump("sin(π/4):", $sin45);
var_dump("log(e):", $log_e);
?>
