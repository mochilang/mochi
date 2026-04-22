<?php
function solve($words) {
    $chars = [];
    foreach ($words as $w) {
        foreach (str_split($w) as $c) $chars[$c] = true;
    }
    ksort($chars);
    $adj = [];
    $indeg = [];
    foreach ($chars as $c => $_) {
        $adj[$c] = [];
        $indeg[$c] = 0;
    }
    for ($i = 0; $i + 1 < count($words); $i++) {
        $a = $words[$i];
        $b = $words[$i + 1];
        $m = min(strlen($a), strlen($b));
        if (substr($a, 0, $m) === substr($b, 0, $m) && strlen($a) > strlen($b)) return "";
        for ($j = 0; $j < $m; $j++) {
            if ($a[$j] !== $b[$j]) {
                if (!isset($adj[$a[$j]][$b[$j]])) {
                    $adj[$a[$j]][$b[$j]] = true;
                    $indeg[$b[$j]]++;
                }
                break;
            }
        }
    }
    $zeros = [];
    foreach ($indeg as $c => $d) if ($d === 0) $zeros[] = $c;
    sort($zeros);
    $out = '';
    while (count($zeros) > 0) {
        $c = array_shift($zeros);
        $out .= $c;
        $nexts = array_keys($adj[$c]);
        sort($nexts);
        foreach ($nexts as $nei) {
            $indeg[$nei]--;
            if ($indeg[$nei] === 0) {
                $zeros[] = $nei;
                sort($zeros);
            }
        }
    }
    return strlen($out) === count($chars) ? $out : "";
}

$lines = preg_split("/\r?\n/", stream_get_contents(STDIN));
if ($lines && trim($lines[0]) !== '') {
    $t = intval(trim($lines[0]));
    $idx = 1;
    $out = [];
    for ($tc = 0; $tc < $t; $tc++) {
        $n = intval(trim($lines[$idx++]));
        $words = [];
        for ($i = 0; $i < $n; $i++) $words[] = trim($lines[$idx++]);
        $out[] = solve($words);
    }
    echo implode("\n", $out);
}
?>
