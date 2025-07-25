<?php
ini_set('memory_limit', '-1');
$creature = 'shark';
$pointer = [$creature];
echo rtrim('creature = ' . $creature), PHP_EOL;
echo rtrim('pointer = 0'), PHP_EOL;
echo rtrim('*pointer = ' . $pointer[0]), PHP_EOL;
$pointer[0] = 'jellyfish';
$creature = $pointer[0];
echo rtrim('*pointer = ' . $pointer[0]), PHP_EOL;
echo rtrim('creature = ' . $creature), PHP_EOL;
