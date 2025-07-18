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
echo "Circle area with r =", $r, "=>", $area, PHP_EOL;
echo "Square root of 49:", $root, PHP_EOL;
echo "sin(π/4):", $sin45, PHP_EOL;
echo "log(e):", $log_e, PHP_EOL;
?>
