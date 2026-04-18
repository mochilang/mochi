<?php
function minWindow($s, $t) {
    $need = array_fill(0, 128, 0);
    $missing = strlen($t);
    for ($i = 0; $i < strlen($t); $i++) $need[ord($t[$i])]++;
    $left = 0; $bestStart = 0; $bestLen = strlen($s) + 1;
    for ($right = 0; $right < strlen($s); $right++) {
        $c = ord($s[$right]);
        if ($need[$c] > 0) $missing--;
        $need[$c]--;
        while ($missing == 0) {
            if ($right - $left + 1 < $bestLen) { $bestStart = $left; $bestLen = $right - $left + 1; }
            $lc = ord($s[$left]);
            $need[$lc]++;
            if ($need[$lc] > 0) $missing++;
            $left++;
        }
    }
    return $bestLen > strlen($s) ? "" : substr($s, $bestStart, $bestLen);
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES); if ($lines) { $t = intval($lines[0]); $out = []; for ($i = 0; $i < $t; $i++) $out[] = minWindow($lines[1 + 2*$i], $lines[2 + 2*$i]); echo implode("\n", $out); }
?>
