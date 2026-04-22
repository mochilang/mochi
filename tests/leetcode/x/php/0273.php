<?php
$less20 = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
    "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"];
$tens = ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"];
$thousands = ["", "Thousand", "Million", "Billion"];

function helper_words($n, $less20, $tens) {
    if ($n === 0) return "";
    if ($n < 20) return $less20[$n];
    if ($n < 100) return $tens[intdiv($n, 10)] . ($n % 10 === 0 ? "" : " " . helper_words($n % 10, $less20, $tens));
    return $less20[intdiv($n, 100)] . " Hundred" . ($n % 100 === 0 ? "" : " " . helper_words($n % 100, $less20, $tens));
}

function solve_num($num, $less20, $tens, $thousands) {
    if ($num === 0) return "Zero";
    $parts = [];
    $idx = 0;
    while ($num > 0) {
        $chunk = $num % 1000;
        if ($chunk !== 0) {
            $words = helper_words($chunk, $less20, $tens);
            if ($thousands[$idx] !== "") $words .= " " . $thousands[$idx];
            array_unshift($parts, $words);
        }
        $num = intdiv($num, 1000);
        $idx++;
    }
    return implode(" ", $parts);
}

$lines = preg_split("/\r?\n/", stream_get_contents(STDIN));
if ($lines && trim($lines[0]) !== '') {
    $t = intval(trim($lines[0]));
    $out = [];
    for ($i = 0; $i < $t; $i++) $out[] = solve_num(intval(trim($lines[$i + 1])), $less20, $tens, $thousands);
    echo implode("\n", $out);
}
?>
