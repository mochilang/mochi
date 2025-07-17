<?php
$math = [
    'sqrt' => function($x) { return sqrt($x); },
    'pow' => function($x, $y) { return pow($x, $y); },
    'sin' => function($x) { return sin($x); },
    'log' => function($x) { return log($x); },
    'pi' => M_PI,
    'e' => M_E,
];
var_dump($math['sqrt'](16));
var_dump($math['pi']);
?>
