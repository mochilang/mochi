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
    $computer = time() % 2 == 0;
    echo "Enter q to quit at any time\n", PHP_EOL;
    if ($computer) {
        echo "The computer will choose first", PHP_EOL;
    } else {
        echo "You will choose first", PHP_EOL;
    }
    echo "\n\nRunning total is now 0\n\n", PHP_EOL;
    $round = 1;
    $done = false;
    while (!$done) {
        echo "ROUND " . strval($round) . ":\n\n", PHP_EOL;
        $i = 0;
        while ($i < 2 && (!$done)) {
            if ($computer) {
                $choice = 0;
                if ($total < 18) {
                    $choice = time() % 3 + 1;
                } else {
                    $choice = 21 - $total;
                }
                $total = $total + $choice;
                echo "The computer chooses " . strval($choice), PHP_EOL;
                echo "Running total is now " . strval($total), PHP_EOL;
                if ($total == 21) {
                    echo "\nSo, commiserations, the computer has won!", PHP_EOL;
                    $done = true;
                }
            } else {
                while (true) {
                    echo "Your choice 1 to 3 : ", PHP_EOL;
                    $line = trim(fgets(STDIN));
                    if ($line == "q" || $line == "Q") {
                        echo "OK, quitting the game", PHP_EOL;
                        $done = true;
                        break;
                    }
                    $num = parseIntStr($line);
                    if ($num < 1 || $num > 3) {
                        if ($total + $num > 21) {
                            echo "Too big, try again", PHP_EOL;
                        } else {
                            echo "Out of range, try again", PHP_EOL;
                        }
                        continue;
                    }
                    if ($total + $num > 21) {
                        echo "Too big, try again", PHP_EOL;
                        continue;
                    }
                    $total = $total + $num;
                    echo "Running total is now " . strval($total), PHP_EOL;
                    break;
                }
                if ($total == 21) {
                    echo "\nSo, congratulations, you've won!", PHP_EOL;
                    $done = true;
                }
            }
            echo "\n", PHP_EOL;
            $computer = !$computer;
            $i = $i + 1;
        }
        $round = $round + 1;
    }
}
main();
?>
