<?php
function shuffle($xs) {
    $arr = $xs;
    $i = 99;
    while ($i > 0) {
        $j = $now() % ($i + 1);
        $tmp = $arr[$i];
        $arr[$i] = $arr[$j];
        $arr[$j] = $tmp;
        $i = $i - 1;
    }
    return $arr;
}
function doTrials($trials, $np, $strategy) {
    $pardoned = 0;
    $t = 0;
    while ($t < $trials) {
        $drawers = [];
        $i = 0;
        while ($i < 100) {
            $drawers = array_merge($drawers, [$i]);
            $i = $i + 1;
        }
        $drawers = shuffle($drawers);
        $p = 0;
        $success = true;
        while ($p < $np) {
            $found = false;
            if ($strategy == "optimal") {
                $prev = $p;
                $d = 0;
                while ($d < 50) {
                    $this = $drawers[$prev];
                    if ($this == $p) {
                        $found = true;
                        break;
                    }
                    $prev = $this;
                    $d = $d + 1;
                }
            } else {
                $opened = [];
                $k = 0;
                while ($k < 100) {
                    $opened = array_merge($opened, [false]);
                    $k = $k + 1;
                }
                $d = 0;
                while ($d < 50) {
                    $n = $now() % 100;
                    while ($opened[$n]) {
                        $n = $now() % 100;
                    }
                    $opened[$n] = true;
                    if ($drawers[$n] == $p) {
                        $found = true;
                        break;
                    }
                    $d = $d + 1;
                }
            }
            if (!$found) {
                $success = false;
                break;
            }
            $p = $p + 1;
        }
        if ($success) {
            $pardoned = $pardoned + 1;
        }
        $t = $t + 1;
    }
    $rf = ((float)($pardoned)) / ((float)($trials)) * 100;
    _print("  strategy = " . $strategy . "  pardoned = " . strval($pardoned) . " relative frequency = " . strval($rf) . "%");
}
function main() {
    $trials = 1000;
    foreach ([10, 100] as $np) {
        _print("Results from " . strval($trials) . " trials with " . strval($np) . " prisoners:\n");
        foreach (["random", "optimal"] as $strat) {
            doTrials($trials, $np, $strat);
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
