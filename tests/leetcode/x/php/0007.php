<?php
function reverseInt(int $x): int {
    $ans = 0;
    while ($x != 0) {
        $digit = $x % 10;
        $x = intdiv($x, 10);
        if ($ans > intdiv(2147483647, 10) || ($ans == intdiv(2147483647, 10) && $digit > 7)) return 0;
        if ($ans < intdiv(-2147483648, 10) || ($ans == intdiv(-2147483648, 10) && $digit < -8)) return 0;
        $ans = $ans * 10 + $digit;
    }
    return $ans;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$t = intval(trim($lines[0]));
$out = [];
for ($i = 0; $i < $t; $i++) {
    $x = intval(trim($lines[$i + 1] ?? '0'));
    $out[] = strval(reverseInt($x));
}
echo implode("\n", $out);
?>
