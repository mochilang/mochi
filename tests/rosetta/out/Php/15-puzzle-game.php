<?php
$board = [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    0
];
$solved = [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    0
];
$empty = 15;
$moves = 0;
$quit = false;
function randMove() {
    return time() % 4;
}
function isSolved() {
    $i = 0;
    while ($i < 16) {
        if ($board[$i] != $solved[$i]) {
            return false;
        }
        $i = $i + 1;
    }
    return true;
}
function isValidMove($m) {
    if ($m == 0) {
        return [
    "idx" => $empty - 4,
    "ok" => $empty / 4 > 0
];
    }
    if ($m == 1) {
        return [
    "idx" => $empty + 4,
    "ok" => $empty / 4 < 3
];
    }
    if ($m == 2) {
        return [
    "idx" => $empty + 1,
    "ok" => $empty % 4 < 3
];
    }
    if ($m == 3) {
        return [
    "idx" => $empty - 1,
    "ok" => $empty % 4 > 0
];
    }
    return ["idx" => 0, "ok" => false];
}
function doMove($m) {
    $r = isValidMove($m);
    if (!$r["ok"]) {
        return false;
    }
    $i = $empty;
    $j = $int($r["idx"]);
    $tmp = $board[$i];
    $board[$i] = $board[$j];
    $board[$j] = $tmp;
    $empty = $j;
    $moves = $moves + 1;
    return true;
}
function _shuffle($n) {
    $i = 0;
    while ($i < $n || isSolved()) {
        if (doMove(randMove())) {
            $i = $i + 1;
        }
    }
}
function printBoard() {
    $line = "";
    $i = 0;
    while ($i < 16) {
        $val = $board[$i];
        if ($val == 0) {
            $line = $line . "  .";
        } else {
            $s = strval($val);
            if ($val < 10) {
                $line = $line . "  " . $s;
            } else {
                $line = $line . " " . $s;
            }
        }
        if ($i % 4 == 3) {
            var_dump($line);
            $line = "";
        }
        $i = $i + 1;
    }
}
function playOneMove() {
    while (true) {
        echo "Enter move #" . strval($moves + 1) . " (U, D, L, R, or Q): ", PHP_EOL;
        $s = trim(fgets(STDIN));
        if ($s == "") {
            continue;
        }
        $c = array_slice($s, 0, 1 - 0);
        $m = 0;
        if ($c == "U" || $c == "u") {
            $m = 0;
        } elseif ($c == "D" || $c == "d") {
            $m = 1;
        } elseif ($c == "R" || $c == "r") {
            $m = 2;
        } elseif ($c == "L" || $c == "l") {
            $m = 3;
        } elseif ($c == "Q" || $c == "q") {
            echo "Quiting after " . strval($moves) . " moves.", PHP_EOL;
            $quit = true;
            return null;
        }
        if (!doMove($m)) {
            echo "That is not a valid move at the moment.", PHP_EOL;
            continue;
        }
        return null;
    }
}
function play() {
    echo "Starting board:", PHP_EOL;
    while ((!$quit && (!isSolved()))) {
        echo "", PHP_EOL;
        printBoard();
        playOneMove();
    }
    if (isSolved()) {
        echo "You solved the puzzle in " . strval($moves) . " moves.", PHP_EOL;
    }
}
function main() {
    _shuffle(50);
    play();
}
main();
?>
