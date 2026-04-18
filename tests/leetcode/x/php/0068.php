<?php
function justify($words, $maxWidth) {
    $res = [];
    $i = 0;
    while ($i < count($words)) {
        $j = $i; $total = 0;
        while ($j < count($words) && $total + strlen($words[$j]) + ($j - $i) <= $maxWidth) { $total += strlen($words[$j]); $j++; }
        $gaps = $j - $i - 1;
        if ($j == count($words) || $gaps == 0) {
            $line = implode(' ', array_slice($words, $i, $j - $i));
            $line .= str_repeat(' ', $maxWidth - strlen($line));
        } else {
            $spaces = $maxWidth - $total; $base = intdiv($spaces, $gaps); $extra = $spaces % $gaps; $line = '';
            for ($k = $i; $k < $j - 1; $k++) { $line .= $words[$k] . str_repeat(' ', $base + (($k - $i < $extra) ? 1 : 0)); }
            $line .= $words[$j - 1];
        }
        $res[] = $line; $i = $j;
    }
    return $res;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES); if ($lines) { $idx = 0; $t = intval($lines[$idx++]); $out = []; for ($tc = 0; $tc < $t; $tc++) { $n = intval($lines[$idx++]); $words = array_slice($lines, $idx, $n); $idx += $n; $width = intval($lines[$idx++]); $ans = justify($words, $width); $out[] = strval(count($ans)); foreach ($ans as $s) $out[] = '|' . $s . '|'; if ($tc + 1 < $t) $out[] = '='; } echo implode("\n", $out); }
?>
