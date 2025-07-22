<?php
$a = 10;
$b = 5;
echo (is_float($a + $b) ? json_encode($a + $b, 1344) : $a + $b), PHP_EOL;
echo (is_float($a - $b) ? json_encode($a - $b, 1344) : $a - $b), PHP_EOL;
echo (is_float($a * $b) ? json_encode($a * $b, 1344) : $a * $b), PHP_EOL;
echo (is_float($a / $b) ? json_encode($a / $b, 1344) : $a / $b), PHP_EOL;
echo (is_float($a % $b) ? json_encode($a % $b, 1344) : $a % $b), PHP_EOL;
