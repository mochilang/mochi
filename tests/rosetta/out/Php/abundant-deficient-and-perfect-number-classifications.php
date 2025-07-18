<?php
function pfacSum($i) {
    $sum = 0;
    $p = 1;
    while ($p <= $i / 2) {
        if ($i % $p == 0) {
            $sum = $sum + $p;
        }
        $p = $p + 1;
    }
    return $sum;
}
function main() {
    $d = 0;
    $a = 0;
    $pnum = 0;
    $i = 1;
    while ($i <= 20000) {
        $j = pfacSum($i);
        if ($j < $i) {
            $d = $d + 1;
        }
        if ($j == $i) {
            $pnum = $pnum + 1;
        }
        if ($j > $i) {
            $a = $a + 1;
        }
        $i = $i + 1;
    }
    echo "There are " . (is_bool($d) ? ($d ? 'true' : 'false') : strval($d)) . " deficient numbers between 1 and 20000", PHP_EOL;
    echo "There are " . (is_bool($a) ? ($a ? 'true' : 'false') : strval($a)) . " abundant numbers  between 1 and 20000", PHP_EOL;
    echo "There are " . (is_bool($pnum) ? ($pnum ? 'true' : 'false') : strval($pnum)) . " perfect numbers between 1 and 20000", PHP_EOL;
}
main();
?>
