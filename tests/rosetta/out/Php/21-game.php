<?php
function parseIntStr($str) {
    $i = 0;
    $neg = false;
    if (strlen($str) > 0 && substr($str, 0, 1 - 0) == "-") {
        $neg = true;
        $i = 1;
    }
    $n = 0;
    $digits = [
    "0" => 0,
    "1" => 1,
    "2" => 2,
    "3" => 3,
    "4" => 4,
    "5" => 5,
    "6" => 6,
    "7" => 7,
    "8" => 8,
    "9" => 9
];
    while ($i < strlen($str)) {
        $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
        $i = $i + 1;
    }
    if ($neg) {
        $n = -$n;
    }
    return $n;
}
function main() {
    $total = 0;
    $computer = $now() % 2 == 0;
    _print("Enter q to quit at any time\n");
    if ($computer) {
        _print("The computer will choose first");
    } else {
        _print("You will choose first");
    }
    _print("\n\nRunning total is now 0\n\n");
    $round = 1;
    $done = false;
    while (!$done) {
        _print("ROUND " . strval($round) . ":\n\n");
        $i = 0;
        while ($i < 2 && (!$done)) {
            if ($computer) {
                $choice = 0;
                if ($total < 18) {
                    $choice = $now() % 3 + 1;
                } else {
                    $choice = 21 - $total;
                }
                $total = $total + $choice;
                _print("The computer chooses " . strval($choice));
                _print("Running total is now " . strval($total));
                if ($total == 21) {
                    _print("\nSo, commiserations, the computer has won!");
                    $done = true;
                }
            } else {
                while (true) {
                    _print("Your choice 1 to 3 : ");
                    $line = $input();
                    if ($line == "q" || $line == "Q") {
                        _print("OK, quitting the game");
                        $done = true;
                        break;
                    }
                    $num = parseIntStr($line);
                    if ($num < 1 || $num > 3) {
                        if ($total + $num > 21) {
                            _print("Too big, try again");
                        } else {
                            _print("Out of range, try again");
                        }
                        continue;
                    }
                    if ($total + $num > 21) {
                        _print("Too big, try again");
                        continue;
                    }
                    $total = $total + $num;
                    _print("Running total is now " . strval($total));
                    break;
                }
                if ($total == 21) {
                    _print("\nSo, congratulations, you've won!");
                    $done = true;
                }
            }
            _print("\n");
            $computer = !$computer;
            $i = $i + 1;
        }
        $round = $round + 1;
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
