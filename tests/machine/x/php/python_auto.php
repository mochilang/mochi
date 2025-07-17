<?php
$math = [
    'sqrt' => function($x) { return sqrt($x); },
    'pow' => function($x, $y) { return pow($x, $y); },
    'sin' => function($x) { return sin($x); },
    'log' => function($x) { return log($x); },
    'pi' => M_PI,
    'e' => M_E,
];
_print($math['sqrt'](16));
_print($math['pi']);
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
