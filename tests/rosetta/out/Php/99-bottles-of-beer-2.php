<?php
function fields($s) {
    $words = [];
    $cur = "";
    $i = 0;
    while ($i < strlen($s)) {
        $ch = substr($s, $i, $i + 1);
        if ($ch == " " || $ch == "\n" || $ch == "\t") {
            if (strlen($cur) > 0) {
                $words = array_merge($words, [$cur]);
                $cur = "";
            }
        } else {
            $cur = $cur . $ch;
        }
        $i = $i + 1;
    }
    if (strlen($cur) > 0) {
        $words = array_merge($words, [$cur]);
    }
    return $words;
}
function _join($xs, $sep) {
    $res = "";
    $i = 0;
    while ($i < count($xs)) {
        if ($i > 0) {
            $res = $res . $sep;
        }
        $res = $res . $xs[$i];
        $i = $i + 1;
    }
    return $res;
}
function numberName($n) {
    $small = [
    "no",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen"
];
    $tens = [
    "ones",
    "ten",
    "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety"
];
    if ($n < 0) {
        return "";
    }
    if ($n < 20) {
        return $small[$n];
    }
    if ($n < 100) {
        $t = $tens[(int)((intdiv($n, 10)))];
        $s = $n % 10;
        if ($s > 0) {
            $t = $t . " " . $small[$s];
        }
        return $t;
    }
    return "";
}
function pluralizeFirst($s, $n) {
    if ($n == 1) {
        return $s;
    }
    $w = fields($s);
    if (count($w) > 0) {
        $w[0] = $w[0] . "s";
    }
    return _join($w, " ");
}
function randInt($seed, $n) {
    $next = ($seed * 1664525 + 1013904223) % 2147483647;
    return $next % $n;
}
function slur($p, $d) {
    if (strlen($p) <= 2) {
        return $p;
    }
    $a = [];
    $i = 1;
    while ($i < strlen($p) - 1) {
        $a = array_merge($a, [substr($p, $i, $i + 1)]);
        $i = $i + 1;
    }
    $idx = count($a) - 1;
    $seed = $d;
    while ($idx >= 1) {
        $seed = ($seed * 1664525 + 1013904223) % 2147483647;
        if ($seed % 100 >= $d) {
            $j = $seed % ($idx + 1);
            $tmp = $a[$idx];
            $a[$idx] = $a[$j];
            $a[$j] = $tmp;
        }
        $idx = $idx - 1;
    }
    $s = substr($p, 0, 1);
    $k = 0;
    while ($k < count($a)) {
        $s = $s . $a[$k];
        $k = $k + 1;
    }
    $s = $s . substr($p, strlen($p) - 1, strlen($p));
    $w = fields($s);
    return _join($w, " ");
}
function main() {
    $i = 99;
    while ($i > 0) {
        var_dump(slur(numberName($i), $i) . " " . pluralizeFirst(slur("bottle of", $i), $i) . " " . slur("beer on the wall", $i));
        var_dump(slur(numberName($i), $i) . " " . pluralizeFirst(slur("bottle of", $i), $i) . " " . slur("beer", $i));
        var_dump(slur("take one", $i) . " " . slur("down", $i) . " " . slur("pass it around", $i));
        var_dump(slur(numberName($i - 1), $i) . " " . pluralizeFirst(slur("bottle of", $i), $i - 1) . " " . slur("beer on the wall", $i));
        $i = $i - 1;
    }
}
main();
?>
