<?php
function myAtoi(string $s): int {
    $i = 0;
    while ($i < strlen($s) && $s[$i] === ' ') $i++;
    $sign = 1;
    if ($i < strlen($s) && ($s[$i] === '+' || $s[$i] === '-')) {
        if ($s[$i] === '-') $sign = -1;
        $i++;
    }
    $ans = 0;
    $limit = $sign > 0 ? 7 : 8;
    while ($i < strlen($s) && $s[$i] >= '0' && $s[$i] <= '9') {
        $digit = ord($s[$i]) - ord('0');
        if ($ans > 214748364 || ($ans == 214748364 && $digit > $limit)) {
            return $sign > 0 ? 2147483647 : -2147483648;
        }
        $ans = $ans * 10 + $digit;
        $i++;
    }
    return $sign * $ans;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$t = intval(trim($lines[0]));
$out = [];
for ($i = 0; $i < $t; $i++) $out[] = strval(myAtoi($lines[$i + 1] ?? ''));
echo implode("\n", $out);
?>
