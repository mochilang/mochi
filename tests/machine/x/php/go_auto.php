<?php
$testpkg = [
    'Add' => function($a, $b) { return $a + $b; },
    'Pi' => 3.14,
    'Answer' => 42,
    'FifteenPuzzleExample' => function() { return 'Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd'; },
];
var_dump($testpkg['Add'](2, 3));
var_dump($testpkg['Pi']);
var_dump($testpkg['Answer']);
?>
