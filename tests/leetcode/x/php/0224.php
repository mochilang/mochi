<?php
function calculate_expr(string $expr): int {
    $result = 0;
    $number = 0;
    $sign = 1;
    $stack = [];
    foreach (str_split($expr) as $ch) {
        if ($ch >= '0' && $ch <= '9') {
            $number = $number * 10 + intval($ch);
        } elseif ($ch === '+' || $ch === '-') {
            $result += $sign * $number;
            $number = 0;
            $sign = $ch === '+' ? 1 : -1;
        } elseif ($ch === '(') {
            $stack[] = $result;
            $stack[] = $sign;
            $result = 0;
            $number = 0;
            $sign = 1;
        } elseif ($ch === ')') {
            $result += $sign * $number;
            $number = 0;
            $prevSign = array_pop($stack);
            $prevResult = array_pop($stack);
            $result = $prevResult + $prevSign * $result;
        }
    }
    return $result + $sign * $number;
}

$lines = preg_split("/\r?\n/", rtrim(stream_get_contents(STDIN), "\r\n"));
if ($lines && trim($lines[0]) !== '') {
    $t = intval(trim($lines[0]));
    $out = [];
    for ($i = 0; $i < $t; $i++) {
        $out[] = strval(calculate_expr($lines[$i + 1] ?? ''));
    }
    echo implode("\n", $out);
}
?>
