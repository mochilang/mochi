<?php
$OP_NUM = 0;
$OP_ADD = 1;
$OP_SUB = 2;
$OP_MUL = 3;
$OP_DIV = 4;
function newNum($n) {
    global $OP_NUM;
        return [
    "op" => $OP_NUM,
    "value" => ["num" => $n, "denom" => 1]
];
}
function exprEval($x) {
    global $OP_ADD, $OP_MUL, $OP_NUM, $OP_SUB;
        if ($x["op"] == $OP_NUM) {
            return $x["value"];
        }
        $l = exprEval($x["left"]);
        $r = exprEval($x["right"]);
        if ($x["op"] == $OP_ADD) {
            return [
    "num" => $l["num"] * $r["denom"] + $l["denom"] * $r["num"],
    "denom" => $l["denom"] * $r["denom"]
];
        }
        if ($x["op"] == $OP_SUB) {
            return [
    "num" => $l["num"] * $r["denom"] - $l["denom"] * $r["num"],
    "denom" => $l["denom"] * $r["denom"]
];
        }
        if ($x["op"] == $OP_MUL) {
            return [
    "num" => $l["num"] * $r["num"],
    "denom" => $l["denom"] * $r["denom"]
];
        }
        return [
    "num" => $l["num"] * $r["denom"],
    "denom" => $l["denom"] * $r["num"]
];
}
function exprString($x) {
    global $OP_ADD, $OP_MUL, $OP_NUM, $OP_SUB;
        if ($x["op"] == $OP_NUM) {
            return (is_bool($x["value"]["num"]) ? ($x["value"]["num"] ? 'true' : 'false') : strval($x["value"]["num"]));
        }
        $ls = exprString($x["left"]);
        $rs = exprString($x["right"]);
        $opstr = "";
        if ($x["op"] == $OP_ADD) {
            $opstr = " + ";
        } elseif ($x["op"] == $OP_SUB) {
            $opstr = " - ";
        } elseif ($x["op"] == $OP_MUL) {
            $opstr = " * ";
        }
        return "(" . $ls . $opstr . $rs . ")";
}
$n_cards = 4;
$goal = 24;
$digit_range = 9;
function solve($xs) {
    global $OP_ADD, $OP_DIV, $OP_MUL, $OP_SUB, $goal;
        if (count($xs) == 1) {
            $f = exprEval($xs[0]);
            if ($f["denom"] != 0 && $f["num"] == $f["denom"] * $goal) {
                echo exprString($xs[0]), PHP_EOL;
                return true;
            }
            return false;
        }
        $i = 0;
        while ($i < count($xs)) {
            $j = $i + 1;
            while ($j < count($xs)) {
                $rest = [];
                $k = 0;
                while ($k < count($xs)) {
                    if ($k != $i && $k != $j) {
                        $rest = array_merge($rest, [$xs[$k]]);
                    }
                    $k = $k + 1;
                }
                $a = $xs[$i];
                $b = $xs[$j];
                foreach ([
    $OP_ADD,
    $OP_SUB,
    $OP_MUL,
    $OP_DIV
] as $op) {
                    $node = [
    "op" => $op,
    "left" => $a,
    "right" => $b
];
                    if (solve(array_merge($rest, [$node]))) {
                        return true;
                    }
                }
                $node = [
    "op" => $OP_SUB,
    "left" => $b,
    "right" => $a
];
                if (solve(array_merge($rest, [$node]))) {
                    return true;
                }
                $node = [
    "op" => $OP_DIV,
    "left" => $b,
    "right" => $a
];
                if (solve(array_merge($rest, [$node]))) {
                    return true;
                }
                $j = $j + 1;
            }
            $i = $i + 1;
        }
        return false;
}
function main() {
    global $digit_range, $n_cards;
        $iter = 0;
        while ($iter < 10) {
            $cards = [];
            $i = 0;
            while ($i < $n_cards) {
                $n = (time() % ($digit_range - 1)) + 1;
                $cards = array_merge($cards, [newNum($n)]);
                echo " " . (is_bool($n) ? ($n ? 'true' : 'false') : strval($n)), PHP_EOL;
                $i = $i + 1;
            }
            echo ":  ", PHP_EOL;
            if (!solve($cards)) {
                echo "No solution", PHP_EOL;
            }
            $iter = $iter + 1;
        }
}
main();
?>
