<?php
$a = 10 - 3;
$b = 2 + 2;
echo (is_float($a) ? json_encode($a, 1344) : $a), PHP_EOL;
echo ($a == 7 ? "True" : "False"), PHP_EOL;
echo ($b < 5 ? "True" : "False"), PHP_EOL;
