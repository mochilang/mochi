<?php
function expandAround(string $s, int $left, int $right): array {
    $n = strlen($s);
    while ($left >= 0 && $right < $n && $s[$left] === $s[$right]) {
        $left--;
        $right++;
    }
    return [$left + 1, $right - $left - 1];
}

function longestPalindrome(string $s): string {
    $bestStart = 0;
    $bestLen = strlen($s) > 0 ? 1 : 0;
    for ($i = 0; $i < strlen($s); $i++) {
        [$start1, $len1] = expandAround($s, $i, $i);
        if ($len1 > $bestLen) {
            $bestStart = $start1;
            $bestLen = $len1;
        }
        [$start2, $len2] = expandAround($s, $i, $i + 1);
        if ($len2 > $bestLen) {
            $bestStart = $start2;
            $bestLen = $len2;
        }
    }
    return substr($s, $bestStart, $bestLen);
}

$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) {
    exit(0);
}
$t = intval(trim($lines[0]));
$out = [];
for ($i = 0; $i < $t; $i++) {
    $s = $lines[$i + 1] ?? '';
    $out[] = longestPalindrome($s);
}
echo implode("\n", $out);
?>
