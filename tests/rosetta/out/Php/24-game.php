<?php
function randDigit() {
    return ($now() % 9) + 1;
}
function main() {
    $digits = [];
    for ($i = 0; $i < 4; $i++) {
        $digits = array_merge($digits, [randDigit()]);
    }
    $numstr = "";
    for ($i = 0; $i < 4; $i++) {
        $numstr = $numstr . strval($digits[$i]);
    }
    _print("Your numbers: " . $numstr . "\n");
    _print("Enter RPN: ");
    $expr = $input();
    if (count($expr) != 7) {
        _print("invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)");
        return null;
    }
    $stack = [];
    $i = 0;
    $valid = true;
    while ($i < count($expr)) {
        $ch = substr($expr, $i, $i + 1);
        if ($ch >= "0" && $ch <= "9") {
            if (count($digits) == 0) {
                _print("too many numbers.");
                return null;
            }
            $j = 0;
            while ($digits[$j] != $int($ch) - $int("0")) {
                $j = $j + 1;
                if ($j == count($digits)) {
                    _print("wrong numbers.");
                    return null;
                }
            }
            $digits = array_slice($digits, 0, $j - 0) + array_slice($digits, $j + 1);
            $stack = array_merge($stack, [float($int($ch) - $int("0"))]);
        } else {
            if (count($stack) < 2) {
                _print("invalid expression syntax.");
                $valid = false;
                break;
            }
            $b = $stack[count($stack) - 1];
            $a = $stack[count($stack) - 2];
            if ($ch == "+") {
                $stack[count($stack) - 2] = $a + $b;
            } elseif ($ch == "-") {
                $stack[count($stack) - 2] = $a - $b;
            } elseif ($ch == "*") {
                $stack[count($stack) - 2] = $a * $b;
            } elseif ($ch == "/") {
                $stack[count($stack) - 2] = $a / $b;
            }
            $stack = array_slice($stack, 0, count($stack) - 1 - 0);
        }
        $i = $i + 1;
    }
    if ($valid) {
        if ($abs($stack[0] - 24) > 1e-06) {
            _print("incorrect. " . strval($stack[0]) . " != 24");
        } else {
            _print("correct.");
        }
    }
}
main();
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
