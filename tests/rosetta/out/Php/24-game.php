<?php
function randDigit() {
    return (time() % 9) + 1;
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
    echo "Your numbers: " . $numstr . "\n", PHP_EOL;
    echo "Enter RPN: ", PHP_EOL;
    $expr = trim(fgets(STDIN));
    if (_len($expr) != 7) {
        echo "invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)", PHP_EOL;
        return null;
    }
    $stack = [];
    $i = 0;
    $valid = true;
    while ($i < _len($expr)) {
        $ch = substr($expr, $i, $i + 1);
        if ($ch >= "0" && $ch <= "9") {
            if (_len($digits) == 0) {
                echo "too many numbers.", PHP_EOL;
                return null;
            }
            $j = 0;
            while ($digits[$j] != $int($ch) - $int("0")) {
                $j = $j + 1;
                if ($j == _len($digits)) {
                    echo "wrong numbers.", PHP_EOL;
                    return null;
                }
            }
            $digits = array_slice($digits, 0, $j - 0) + array_slice($digits, $j + 1);
            $stack = array_merge($stack, [float($int($ch) - $int("0"))]);
        } else {
            if (_len($stack) < 2) {
                echo "invalid expression syntax.", PHP_EOL;
                $valid = false;
                break;
            }
            $b = $stack[_len($stack) - 1];
            $a = $stack[_len($stack) - 2];
            if ($ch == "+") {
                $stack[_len($stack) - 2] = $a + $b;
            } elseif ($ch == "-") {
                $stack[_len($stack) - 2] = $a - $b;
            } elseif ($ch == "*") {
                $stack[_len($stack) - 2] = $a * $b;
            } elseif ($ch == "/") {
                $stack[_len($stack) - 2] = $a / $b;
            }
            $stack = array_slice($stack, 0, _len($stack) - 1 - 0);
        }
        $i = $i + 1;
    }
    if ($valid) {
        if ($abs($stack[0] - 24) > 1e-06) {
            echo "incorrect. " . strval($stack[0]) . " != 24", PHP_EOL;
        } else {
            echo "correct.", PHP_EOL;
        }
    }
}
main();
function _len($v) {
    if (is_array($v) && array_key_exists('items', $v)) {
        return count($v['items']);
    }
    if (is_object($v) && property_exists($v, 'items')) {
        return count($v->items);
    }
    if (is_array($v)) {
        return count($v);
    }
    if (is_string($v)) {
        return strlen($v);
    }
    return 0;
}
?>
