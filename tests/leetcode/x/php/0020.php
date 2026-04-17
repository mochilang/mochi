<?php
$data = trim(stream_get_contents(STDIN));
if ($data === '') exit(0);
$tokens = preg_split('/\s+/', $data);

function is_valid($s) {
    $stack = [];
    $n = strlen($s);
    for ($i = 0; $i < $n; $i++) {
        $ch = $s[$i];
        if ($ch === '(' || $ch === '[' || $ch === '{') {
            $stack[] = $ch;
        } else {
            if (count($stack) === 0) return false;
            $open = array_pop($stack);
            if (($ch === ')' && $open !== '(') ||
                ($ch === ']' && $open !== '[') ||
                ($ch === '}' && $open !== '{')) return false;
        }
    }
    return count($stack) === 0;
}

$t = intval($tokens[0]);
for ($i = 0; $i < $t; $i++) {
    echo is_valid($tokens[$i + 1]) ? 'true' : 'false';
    if ($i + 1 < $t) echo PHP_EOL;
}
