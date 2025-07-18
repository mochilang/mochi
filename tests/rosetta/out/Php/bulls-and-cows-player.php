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
function fields($s) {
    $words = [];
    $cur = "";
    $i = 0;
    while ($i < strlen($s)) {
        $ch = substr($s, $i, $i + 1);
        if ($ch == " " || $ch == "\t" || $ch == "\n") {
            if (_len($cur) > 0) {
                $words = array_merge($words, [$cur]);
                $cur = "";
            }
        } else {
            $cur = $cur + $ch;
        }
        $i = $i + 1;
    }
    if (_len($cur) > 0) {
        $words = array_merge($words, [$cur]);
    }
    return $words;
}
function makePatterns() {
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
    $pats = [];
    $i = 0;
    while ($i < _len($digits)) {
        $j = 0;
        while ($j < _len($digits)) {
            if ($j != $i) {
                $k = 0;
                while ($k < _len($digits)) {
                    if ($k != $i && $k != $j) {
                        $l = 0;
                        while ($l < _len($digits)) {
                            if ($l != $i && $l != $j && $l != $k) {
                                $pats = array_merge($pats, [$digits[$i] + $digits[$j] + $digits[$k] + $digits[$l]]);
                            }
                            $l = $l + 1;
                        }
                    }
                    $k = $k + 1;
                }
            }
            $j = $j + 1;
        }
        $i = $i + 1;
    }
    return $pats;
}
function main() {
    echo "Cows and bulls/player\n" . "You think of four digit number of unique digits in the range 1 to 9.\n" . "I guess.  You score my guess:\n" . "    A correct digit but not in the correct place is a cow.\n" . "    A correct digit in the correct place is a bull.\n" . "You give my score as two numbers separated with a space.", PHP_EOL;
    $patterns = makePatterns();
    while (true) {
        if (_len($patterns) == 0) {
            echo "Oops, check scoring.", PHP_EOL;
            return null;
        }
        $guess = $patterns[0];
        $patterns = array_slice($patterns, 1);
        $cows = 0;
        $bulls = 0;
        while (true) {
            echo "My guess: " . $guess . ".  Score? (c b) ", PHP_EOL;
            $line = trim(fgets(STDIN));
            $toks = fields($line);
            if (_len($toks) == 2) {
                $c = $int($toks[0]);
                $b = $int($toks[1]);
                if ($c >= 0 && $c <= 4 && $b >= 0 && $b <= 4 && $c + $b <= 4) {
                    $cows = $c;
                    $bulls = $b;
                    break;
                }
            }
            echo "Score guess as two numbers: cows bulls", PHP_EOL;
        }
        if ($bulls == 4) {
            echo "I did it. :)", PHP_EOL;
            return null;
        }
        $next = [];
        $idx = 0;
        while ($idx < _len($patterns)) {
            $pat = $patterns[$idx];
            $c = 0;
            $b = 0;
            $i = 0;
            while ($i < 4) {
                $cg = substr($guess, $i, $i + 1);
                $cp = substr($pat, $i, $i + 1);
                if ($cg == $cp) {
                    $b = $b + 1;
                } elseif (indexOf($pat, $cg) >= 0) {
                    $c = $c + 1;
                }
                $i = $i + 1;
            }
            if ($c == $cows && $b == $bulls) {
                $next = array_merge($next, [$pat]);
            }
            $idx = $idx + 1;
        }
        $patterns = $next;
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
