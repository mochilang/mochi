<?php
function dfs($s, $i, $left, $right, $balance, &$path, &$ans) {
    if ($i === strlen($s)) {
        if ($left === 0 && $right === 0 && $balance === 0) $ans[$path] = true;
        return;
    }
    $ch = $s[$i];
    if ($ch === '(') {
        if ($left > 0) dfs($s, $i + 1, $left - 1, $right, $balance, $path, $ans);
        $path .= $ch;
        dfs($s, $i + 1, $left, $right, $balance + 1, $path, $ans);
        $path = substr($path, 0, -1);
    } elseif ($ch === ')') {
        if ($right > 0) dfs($s, $i + 1, $left, $right - 1, $balance, $path, $ans);
        if ($balance > 0) {
            $path .= $ch;
            dfs($s, $i + 1, $left, $right, $balance - 1, $path, $ans);
            $path = substr($path, 0, -1);
        }
    } else {
        $path .= $ch;
        dfs($s, $i + 1, $left, $right, $balance, $path, $ans);
        $path = substr($path, 0, -1);
    }
}

function solve($s) {
    $leftRemove = 0;
    $rightRemove = 0;
    for ($i = 0; $i < strlen($s); $i++) {
        $ch = $s[$i];
        if ($ch === '(') $leftRemove++;
        elseif ($ch === ')') {
            if ($leftRemove > 0) $leftRemove--;
            else $rightRemove++;
        }
    }
    $ans = [];
    $path = "";
    dfs($s, 0, $leftRemove, $rightRemove, 0, $path, $ans);
    $res = array_keys($ans);
    sort($res, SORT_STRING);
    return $res;
}

$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if (!$lines) exit(0);
$t = intval(trim($lines[0]));
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $ans = solve($lines[$tc + 1]);
    $blocks[] = implode("\n", array_merge([strval(count($ans))], $ans));
}
echo implode("\n\n", $blocks);
