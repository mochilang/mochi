<?php
function divisors($n) {
    $divs = [1];
    $divs2 = [];
    $i = 2;
    while ($i * $i <= $n) {
        if ($n % $i == 0) {
            $j = (int)((intdiv($n, $i)));
            $divs = array_merge($divs, [$i]);
            if ($i != $j) {
                $divs2 = array_merge($divs2, [$j]);
            }
        }
        $i = $i + 1;
    }
    $j = count($divs2) - 1;
    while ($j >= 0) {
        $divs = array_merge($divs, [$divs2[$j]]);
        $j = $j - 1;
    }
    return $divs;
}
function sum($xs) {
    $tot = 0;
    foreach ($xs as $v) {
        $tot = $tot + $v;
    }
    return $tot;
}
function sumStr($xs) {
    $s = "";
    $i = 0;
    while ($i < count($xs)) {
        $s = $s . (is_bool($xs[$i]) ? ($xs[$i] ? 'true' : 'false') : strval($xs[$i])) . " + ";
        $i = $i + 1;
    }
    return substr($s, 0, strlen($s) - 3);
}
function pad2($n) {
    $s = (is_bool($n) ? ($n ? 'true' : 'false') : strval($n));
    if (strlen($s) < 2) {
        return " " . $s;
    }
    return $s;
}
function pad5($n) {
    $s = (is_bool($n) ? ($n ? 'true' : 'false') : strval($n));
    while (strlen($s) < 5) {
        $s = " " . $s;
    }
    return $s;
}
function abundantOdd($searchFrom, $countFrom, $countTo, $printOne) {
    $count = $countFrom;
    $n = $searchFrom;
    while ($count < $countTo) {
        $divs = divisors($n);
        $tot = array_sum($divs);
        if ($tot > $n) {
            $count = $count + 1;
            if ($printOne && $count < $countTo) {
                $n = $n + 2;
                continue;
            }
            $s = sumStr($divs);
            if (!$printOne) {
                echo pad2($count) . ". " . pad5($n) . " < " . $s . " = " . (is_bool($tot) ? ($tot ? 'true' : 'false') : strval($tot)), PHP_EOL;
            } else {
                echo (is_bool($n) ? ($n ? 'true' : 'false') : strval($n)) . " < " . $s . " = " . (is_bool($tot) ? ($tot ? 'true' : 'false') : strval($tot)), PHP_EOL;
            }
        }
        $n = $n + 2;
    }
    return $n;
}
function main() {
    $max = 25;
    echo "The first " . (is_bool($max) ? ($max ? 'true' : 'false') : strval($max)) . " abundant odd numbers are:", PHP_EOL;
    $n = abundantOdd(1, 0, $max, false);
    echo "\nThe one thousandth abundant odd number is:", PHP_EOL;
    abundantOdd($n, $max, 1000, true);
    echo "\nThe first abundant odd number above one billion is:", PHP_EOL;
    abundantOdd(1000000001, 0, 1, true);
}
main();
?>
