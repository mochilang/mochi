<?php
$testpkg = [
    'Add' => function($a, $b) { return $a + $b; },
    'Pi' => 3.14,
    'Answer' => 42,
    'FifteenPuzzleExample' => function() { return 'Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd'; },
];
_print($testpkg['Add'](2, 3));
_print($testpkg['Pi']);
_print($testpkg['Answer']);
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
