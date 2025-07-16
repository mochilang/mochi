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
_print("Circle area with r =", $r, "=>", $area);
_print("Square root of 49:", $root);
_print("sin(π/4):", $sin45);
_print("log(e):", $log_e);
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
