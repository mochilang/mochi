<?php
function indexOf($s, $ch) {
    $i = 0;
    while ($i < strlen($s)) {
        if (substr($s, $i, $i + 1) == $ch) {
            return $i;
        }
        $i = $i + 1;
    }
    return -1;
}
function _shuffle($xs) {
    $arr = $xs;
    $i = _len($arr) - 1;
    while ($i > 0) {
        $j = time() % ($i + 1);
        $tmp = $arr[$i];
        $arr[$i] = $arr[$j];
        $arr[$j] = $tmp;
        $i = $i - 1;
    }
    return $arr;
}
function main() {
    echo "Cows and Bulls", PHP_EOL;
    echo "Guess four digit number of unique digits in the range 1 to 9.", PHP_EOL;
    echo "A correct digit but not in the correct place is a cow.", PHP_EOL;
    echo "A correct digit in the correct place is a bull.", PHP_EOL;
    $digits = [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9"
];
    $digits = _shuffle($digits);
    $pat = $digits[0] + $digits[1] + $digits[2] + $digits[3];
    $valid = "123456789";
    while (true) {
        echo "Guess: ", PHP_EOL;
        $guess = trim(fgets(STDIN));
        if (_len($guess) != 4) {
            echo "Please guess a four digit number.", PHP_EOL;
            continue;
        }
        $cows = 0;
        $bulls = 0;
        $seen = "";
        $i = 0;
        $malformed = false;
        while ($i < 4) {
            $cg = substr($guess, $i, $i + 1);
            if (indexOf($seen, $cg) != (-1)) {
                echo "Repeated digit: " . $cg, PHP_EOL;
                $malformed = true;
                break;
            }
            $seen = $seen + $cg;
            $pos = indexOf($pat, $cg);
            if ($pos == (-1)) {
                if (indexOf($valid, $cg) == (-1)) {
                    echo "Invalid digit: " . $cg, PHP_EOL;
                    $malformed = true;
                    break;
                }
            } else {
                if ($pos == $i) {
                    $bulls = $bulls + 1;
                } else {
                    $cows = $cows + 1;
                }
            }
            $i = $i + 1;
        }
        if ($malformed) {
            continue;
        }
        echo "Cows: " . strval($cows) . ", bulls: " . strval($bulls), PHP_EOL;
        if ($bulls == 4) {
            echo "You got it.", PHP_EOL;
            break;
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
