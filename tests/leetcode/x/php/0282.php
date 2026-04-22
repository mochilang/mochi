<?php
function solve($num, $target) {
    $ans = [];
    $n = strlen($num);
    $dfs = function($i, $expr, $value, $last) use ($num, $target, $n, &$ans, &$dfs) {
        if ($i === $n) {
            if ($value === $target) $ans[] = $expr;
            return;
        }
        for ($j = $i; $j < $n; $j++) {
            if ($j > $i && $num[$i] === '0') break;
            $s = substr($num, $i, $j - $i + 1);
            $val = intval($s);
            if ($i === 0) {
                $dfs($j + 1, $s, $val, $val);
            } else {
                $dfs($j + 1, $expr . '+' . $s, $value + $val, $val);
                $dfs($j + 1, $expr . '-' . $s, $value - $val, -$val);
                $dfs($j + 1, $expr . '*' . $s, $value - $last + $last * $val, $last * $val);
            }
        }
    };
    $dfs(0, '', 0, 0);
    sort($ans, SORT_STRING);
    return $ans;
}

$lines = preg_split("/\r?\n/", stream_get_contents(STDIN));
if ($lines && trim($lines[0]) !== '') {
    $t = intval(trim($lines[0]));
    $idx = 1;
    $blocks = [];
    for ($tc = 0; $tc < $t; $tc++) {
        $num = trim($lines[$idx++]);
        $target = intval(trim($lines[$idx++]));
        $ans = solve($num, $target);
        $blocks[] = implode("\n", array_merge([strval(count($ans))], $ans));
    }
    echo implode("\n\n", $blocks);
}
?>
