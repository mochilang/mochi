<?php
// Solution for SPOJ TEST - Life, the Universe, and Everything
// https://www.spoj.com/problems/TEST
$handle = fopen("php://stdin", "r");
while (($line = fgets($handle)) !== false) {
    $n = intval(trim($line));
    if ($n === 42) {
        break;
    }
    echo $n, PHP_EOL;
}

