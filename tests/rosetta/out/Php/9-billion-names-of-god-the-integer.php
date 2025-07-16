<?php
function bigTrim($a) {
    $n = count($a);
    while ($n > 1 && $a[$n - 1] == 0) {
        $a = array_slice($a, 0, $n - 1 - 0);
        $n = $n - 1;
    }
    return $a;
}
function bigFromInt($x) {
    if ($x == 0) {
        return [0];
    }
    $digits = [];
    $n = $x;
    while ($n > 0) {
        $digits = array_merge($digits, [$n % 10]);
        $n = $n / 10;
    }
    return $digits;
}
function bigAdd($a, $b) {
    $res = [];
    $carry = 0;
    $i = 0;
    while ($i < count($a) || $i < count($b) || $carry > 0) {
        $av = 0;
        if ($i < count($a)) {
            $av = $a[$i];
        }
        $bv = 0;
        if ($i < count($b)) {
            $bv = $b[$i];
        }
        $s = $av + $bv + $carry;
        $res = array_merge($res, [$s % 10]);
        $carry = $s / 10;
        $i = $i + 1;
    }
    return bigTrim($res);
}
function bigSub($a, $b) {
    $res = [];
    $borrow = 0;
    $i = 0;
    while ($i < count($a)) {
        $av = $a[$i];
        $bv = 0;
        if ($i < count($b)) {
            $bv = $b[$i];
        }
        $diff = $av - $bv - $borrow;
        if ($diff < 0) {
            $diff = $diff + 10;
            $borrow = 1;
        } else {
            $borrow = 0;
        }
        $res = array_merge($res, [$diff]);
        $i = $i + 1;
    }
    return bigTrim($res);
}
function bigToString($a) {
    $s = "";
    $i = count($a) - 1;
    while ($i >= 0) {
        $s = $s . strval($a[$i]);
        $i = $i - 1;
    }
    return $s;
}
function minInt($a, $b) {
    if ($a < $b) {
        return $a;
    } else {
        return $b;
    }
}
function cumu($n) {
    $cache = [[bigFromInt(1)]];
    $y = 1;
    while ($y <= $n) {
        $row = [bigFromInt(0)];
        $x = 1;
        while ($x <= $y) {
            $val = $cache[$y - $x][minInt($x, $y - $x)];
            $row = array_merge($row, [bigAdd($row[count($row) - 1], $val)]);
            $x = $x + 1;
        }
        $cache = array_merge($cache, [$row]);
        $y = $y + 1;
    }
    return $cache[$n];
}
function row($n) {
    $e = cumu($n);
    $out = [];
    $i = 0;
    while ($i < $n) {
        $diff = bigSub($e[$i + 1], $e[$i]);
        $out = array_merge($out, [bigToString($diff)]);
        $i = $i + 1;
    }
    return $out;
}
_print("rows:");
$x = 1;
while ($x < 11) {
    $r = row($x);
    $line = "";
    $i = 0;
    while ($i < count($r)) {
        $line = $line . " " . $r[$i] . " ";
        $i = $i + 1;
    }
    _print($line);
    $x = $x + 1;
}
_print("");
_print("sums:");
foreach ([23, 123, 1234] as $num) {
    $r = cumu($num);
    _print(strval($num) . " " . bigToString($r[count($r) - 1]));
}
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
