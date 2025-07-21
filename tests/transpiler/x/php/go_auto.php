<?php
$testpkg = ["Add" => function($a, $b) {
  return $a + $b;
}, "Pi" => 3.14, "Answer" => 42, "FifteenPuzzleExample" => function() {
  return "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd";
}];
echo rtrim((is_float($testpkg['Add'](2, 3)) ? json_encode($testpkg['Add'](2, 3), 1344) : $testpkg['Add'](2, 3))), PHP_EOL;
echo rtrim((is_float($testpkg["Pi"]) ? json_encode($testpkg["Pi"], 1344) : $testpkg["Pi"])), PHP_EOL;
echo rtrim((is_float($testpkg["Answer"]) ? json_encode($testpkg["Answer"], 1344) : $testpkg["Answer"])), PHP_EOL;
?>
