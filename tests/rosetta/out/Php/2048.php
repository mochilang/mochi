<?php
$SIZE = 4;
function newBoard() {
    $b = [];
    $y = 0;
    while ($y < $SIZE) {
        $row = [];
        $x = 0;
        while ($x < $SIZE) {
            $row = array_merge($row, [0]);
            $x = $x + 1;
        }
        $b = array_merge($b, [$row]);
        $y = $y + 1;
    }
    return $b;
}
function spawnTile($b) {
    $empty = [];
    $y = 0;
    while ($y < $SIZE) {
        $x = 0;
        while ($x < $SIZE) {
            if ($b[$y][$x] == 0) {
                $empty = array_merge($empty, [[$x, $y]]);
            }
            $x = $x + 1;
        }
        $y = $y + 1;
    }
    if (count($empty) == 0) {
        return ["board" => $b, "full" => true];
    }
    $idx = $now() % count($empty);
    $cell = $empty[$idx];
    $val = 4;
    if ($now() % 10 < 9) {
        $val = 2;
    }
    $b[$cell[1]][$cell[0]] = $val;
    return [
    "board" => $b,
    "full" => count($empty) == 1
];
}
function pad($n) {
    $s = strval($n);
    $pad = 4 - count($s);
    $i = 0;
    $out = "";
    while ($i < $pad) {
        $out = $out . " ";
        $i = $i + 1;
    }
    return $out + $s;
}
function draw($b, $score) {
    _print("Score: " . strval($score));
    $y = 0;
    while ($y < $SIZE) {
        _print("+----+----+----+----+");
        $line = "|";
        $x = 0;
        while ($x < $SIZE) {
            $v = $b[$y][$x];
            if ($v == 0) {
                $line = $line . "    |";
            } else {
                $line = $line . pad($v) . "|";
            }
            $x = $x + 1;
        }
        _print($line);
        $y = $y + 1;
    }
    _print("+----+----+----+----+");
    _print("W=Up S=Down A=Left D=Right Q=Quit");
}
function reverseRow($r) {
    $out = [];
    $i = count($r) - 1;
    while ($i >= 0) {
        $out = array_merge($out, [$r[$i]]);
        $i = $i - 1;
    }
    return $out;
}
function slideLeft($row) {
    $xs = [];
    $i = 0;
    while ($i < count($row)) {
        if ($row[$i] != 0) {
            $xs = array_merge($xs, [$row[$i]]);
        }
        $i = $i + 1;
    }
    $res = [];
    $gain = 0;
    $i = 0;
    while ($i < count($xs)) {
        if ($i + 1 < count($xs) && $xs[$i] == $xs[$i + 1]) {
            $v = $xs[$i] * 2;
            $gain = $gain + $v;
            $res = array_merge($res, [$v]);
            $i = $i + 2;
        } else {
            $res = array_merge($res, [$xs[$i]]);
            $i = $i + 1;
        }
    }
    while (count($res) < $SIZE) {
        $res = array_merge($res, [0]);
    }
    return ["row" => $res, "gain" => $gain];
}
function moveLeft($b, $score) {
    $moved = false;
    $y = 0;
    while ($y < $SIZE) {
        $r = slideLeft($b[$y]);
        $new = $r["row"];
        $score = $score + $r["gain"];
        $x = 0;
        while ($x < $SIZE) {
            if ($b[$y][$x] != $new[$x]) {
                $moved = true;
            }
            $b[$y][$x] = $new[$x];
            $x = $x + 1;
        }
        $y = $y + 1;
    }
    return [
    "board" => $b,
    "score" => $score,
    "moved" => $moved
];
}
function moveRight($b, $score) {
    $moved = false;
    $y = 0;
    while ($y < $SIZE) {
        $rev = reverseRow($b[$y]);
        $r = slideLeft($rev);
        $rev = $r["row"];
        $score = $score + $r["gain"];
        $rev = reverseRow($rev);
        $x = 0;
        while ($x < $SIZE) {
            if ($b[$y][$x] != $rev[$x]) {
                $moved = true;
            }
            $b[$y][$x] = $rev[$x];
            $x = $x + 1;
        }
        $y = $y + 1;
    }
    return [
    "board" => $b,
    "score" => $score,
    "moved" => $moved
];
}
function getCol($b, $x) {
    $col = [];
    $y = 0;
    while ($y < $SIZE) {
        $col = array_merge($col, [$b[$y][$x]]);
        $y = $y + 1;
    }
    return $col;
}
function setCol($b, $x, $col) {
    $y = 0;
    while ($y < $SIZE) {
        $b[$y][$x] = $col[$y];
        $y = $y + 1;
    }
}
function moveUp($b, $score) {
    $moved = false;
    $x = 0;
    while ($x < $SIZE) {
        $col = getCol($b, $x);
        $r = slideLeft($col);
        $new = $r["row"];
        $score = $score + $r["gain"];
        $y = 0;
        while ($y < $SIZE) {
            if ($b[$y][$x] != $new[$y]) {
                $moved = true;
            }
            $b[$y][$x] = $new[$y];
            $y = $y + 1;
        }
        $x = $x + 1;
    }
    return [
    "board" => $b,
    "score" => $score,
    "moved" => $moved
];
}
function moveDown($b, $score) {
    $moved = false;
    $x = 0;
    while ($x < $SIZE) {
        $col = reverseRow(getCol($b, $x));
        $r = slideLeft($col);
        $col = $r["row"];
        $score = $score + $r["gain"];
        $col = reverseRow($col);
        $y = 0;
        while ($y < $SIZE) {
            if ($b[$y][$x] != $col[$y]) {
                $moved = true;
            }
            $b[$y][$x] = $col[$y];
            $y = $y + 1;
        }
        $x = $x + 1;
    }
    return [
    "board" => $b,
    "score" => $score,
    "moved" => $moved
];
}
function hasMoves($b) {
    $y = 0;
    while ($y < $SIZE) {
        $x = 0;
        while ($x < $SIZE) {
            if ($b[$y][$x] == 0) {
                return true;
            }
            if ($x + 1 < $SIZE && $b[$y][$x] == $b[$y][$x + 1]) {
                return true;
            }
            if ($y + 1 < $SIZE && $b[$y][$x] == $b[$y + 1][$x]) {
                return true;
            }
            $x = $x + 1;
        }
        $y = $y + 1;
    }
    return false;
}
function has2048($b) {
    $y = 0;
    while ($y < $SIZE) {
        $x = 0;
        while ($x < $SIZE) {
            if ($b[$y][$x] >= 2048) {
                return true;
            }
            $x = $x + 1;
        }
        $y = $y + 1;
    }
    return false;
}
$board = newBoard();
$r = spawnTile($board);
$board = $r["board"];
$full = $r["full"];
$r = spawnTile($board);
$board = $r["board"];
$full = $r["full"];
$score = 0;
draw($board, $score);
while (true) {
    _print("Move: ");
    $cmd = $input();
    $moved = false;
    if ($cmd == "a" || $cmd == "A") {
        $m = moveLeft($board, $score);
        $board = $m["board"];
        $score = $m["score"];
        $moved = $m["moved"];
    }
    if ($cmd == "d" || $cmd == "D") {
        $m = moveRight($board, $score);
        $board = $m["board"];
        $score = $m["score"];
        $moved = $m["moved"];
    }
    if ($cmd == "w" || $cmd == "W") {
        $m = moveUp($board, $score);
        $board = $m["board"];
        $score = $m["score"];
        $moved = $m["moved"];
    }
    if ($cmd == "s" || $cmd == "S") {
        $m = moveDown($board, $score);
        $board = $m["board"];
        $score = $m["score"];
        $moved = $m["moved"];
    }
    if ($cmd == "q" || $cmd == "Q") {
        break;
    }
    if ($moved) {
        $r2 = spawnTile($board);
        $board = $r2["board"];
        $full = $r2["full"];
        if ($full && (!hasMoves($board))) {
            draw($board, $score);
            _print("Game Over");
            break;
        }
    }
    draw($board, $score);
    if (has2048($board)) {
        _print("You win!");
        break;
    }
    if (!hasMoves($board)) {
        _print("Game Over");
        break;
    }
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
