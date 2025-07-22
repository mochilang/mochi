<?php
$testpkg = ["Add" => function($a, $b) {
  return $a + $b;
}, "Pi" => 3.14, "Answer" => 42, "FifteenPuzzleExample" => function() {
  return "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd";
}];
echo json_encode($testpkg['FifteenPuzzleExample'](), 1344), PHP_EOL;
