<?php
function convertZigzag(string $s, int $numRows): string {
    $n = strlen($s);
    if ($numRows <= 1 || $numRows >= $n) return $s;
    $cycle = 2 * $numRows - 2;
    $out = '';
    for ($row = 0; $row < $numRows; $row++) {
        for ($i = $row; $i < $n; $i += $cycle) {
            $out .= $s[$i];
            $diag = $i + $cycle - 2 * $row;
            if ($row > 0 && $row < $numRows - 1 && $diag < $n) $out .= $s[$diag];
        }
    }
    return $out;
}

$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$t = intval(trim($lines[0]));
$out = [];
$idx = 1;
for ($i = 0; $i < $t; $i++) {
    $s = $lines[$idx] ?? '';
    $idx++;
    $numRows = intval(trim($lines[$idx] ?? '1'));
    $idx++;
    $out[] = convertZigzag($s, $numRows);
}
echo implode("\n", $out);
?>
