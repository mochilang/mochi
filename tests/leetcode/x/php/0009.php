<?php
$data = trim(stream_get_contents(STDIN));
if ($data === '') exit(0);
$tokens = preg_split('/\s+/', $data);
$idx = 0;
$t = intval($tokens[$idx++]);

function is_palindrome($x) {
    if ($x < 0) return false;
    $original = $x;
    $rev = 0;
    while ($x > 0) {
        $rev = $rev * 10 + ($x % 10);
        $x = intdiv($x, 10);
    }
    return $rev === $original;
}

$out = [];
for ($i = 0; $i < $t; $i++) {
    $out[] = is_palindrome(intval($tokens[$idx++])) ? 'true' : 'false';
}
echo implode(PHP_EOL, $out);
