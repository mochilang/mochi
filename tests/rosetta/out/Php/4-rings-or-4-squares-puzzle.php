<?php
function validComb($a, $b, $c, $d, $e, $f, $g) {
    $square1 = $a + $b;
    $square2 = $b + $c + $d;
    $square3 = $d + $e + $f;
    $square4 = $f + $g;
    return $square1 == $square2 && $square2 == $square3 && $square3 == $square4;
}
function isUnique($a, $b, $c, $d, $e, $f, $g) {
    $nums = [
    $a,
    $b,
    $c,
    $d,
    $e,
    $f,
    $g
];
    $i = 0;
    while ($i < count($nums)) {
        $j = $i + 1;
        while ($j < count($nums)) {
            if ($nums[$i] == $nums[$j]) {
                return false;
            }
            $j = $j + 1;
        }
        $i = $i + 1;
    }
    return true;
}
function getCombs($low, $high, $unique) {
    $valid = [];
    $count = 0;
    for ($b = $low; $b < ($high + 1); $b++) {
        for ($c = $low; $c < ($high + 1); $c++) {
            for ($d = $low; $d < ($high + 1); $d++) {
                $s = $b + $c + $d;
                for ($e = $low; $e < ($high + 1); $e++) {
                    for ($f = $low; $f < ($high + 1); $f++) {
                        $a = $s - $b;
                        $g = $s - $f;
                        if ($a < $low || $a > $high) {
                            continue;
                        }
                        if ($g < $low || $g > $high) {
                            continue;
                        }
                        if ($d + $e + $f != $s) {
                            continue;
                        }
                        if ($f + $g != $s) {
                            continue;
                        }
                        if (!$unique || isUnique($a, $b, $c, $d, $e, $f, $g)) {
                            $valid = array_merge($valid, [[
    $a,
    $b,
    $c,
    $d,
    $e,
    $f,
    $g
]]);
                            $count = $count + 1;
                        }
                    }
                }
            }
        }
    }
    return ["count" => $count, "list" => $valid];
}
$r1 = getCombs(1, 7, true);
echo strval($r1["count"]) . " unique solutions in 1 to 7", "\n";
echo $r1["list"], "\n";
$r2 = getCombs(3, 9, true);
echo strval($r2["count"]) . " unique solutions in 3 to 9", "\n";
echo $r2["list"], "\n";
$r3 = getCombs(0, 9, false);
echo strval($r3["count"]) . " non-unique solutions in 0 to 9", "\n";
?>
