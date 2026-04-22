<?php
$pairs = [['0', '0'], ['1', '1'], ['6', '9'], ['8', '8'], ['9', '6']];

function build_nums($n, $m, $pairs) {
    if ($n === 0) return [''];
    if ($n === 1) return ['0', '1', '8'];
    $mids = build_nums($n - 2, $m, $pairs);
    $res = [];
    foreach ($mids as $mid) {
        foreach ($pairs as [$a, $b]) {
            if ($n === $m && $a === '0') continue;
            $res[] = $a . $mid . $b;
        }
    }
    return $res;
}

function count_range($low, $high, $pairs) {
    $ans = 0;
    for ($len = strlen($low); $len <= strlen($high); $len++) {
        foreach (build_nums($len, $len, $pairs) as $s) {
            if ($len === strlen($low) && strcmp($s, $low) < 0) continue;
            if ($len === strlen($high) && strcmp($s, $high) > 0) continue;
            $ans++;
        }
    }
    return $ans;
}

$lines = preg_split("/\r?\n/", trim(stream_get_contents(STDIN)));
if ($lines && trim($lines[0]) !== '') {
    $t = intval(trim($lines[0]));
    $out = [];
    $idx = 1;
    for ($i = 0; $i < $t; $i++) {
        $out[] = strval(count_range(trim($lines[$idx++]), trim($lines[$idx++]), $pairs));
    }
    echo implode("\n", $out);
}
?>
