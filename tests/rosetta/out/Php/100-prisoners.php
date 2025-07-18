<?php
function _shuffle($xs) {
    $arr = $xs;
    $i = 99;
    while ($i > 0) {
        $j = time() % ($i + 1);
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
        $drawers = _shuffle($drawers);
        $p = 0;
        $success = true;
        while ($p < $np) {
            $found = false;
            if ($strategy == "optimal") {
                $prev = $p;
                $d = 0;
                while ($d < 50) {
                    $_this = $drawers[$prev];
                    if ($_this == $p) {
                        $found = true;
                        break;
                    }
                    $prev = $_this;
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
                    $n = time() % 100;
                    while ($opened[$n]) {
                        $n = time() % 100;
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
    echo "  strategy = " . $strategy . "  pardoned = " . strval($pardoned) . " relative frequency = " . strval($rf) . "%", PHP_EOL;
}
function main() {
    $trials = 1000;
    foreach ([10, 100] as $np) {
        echo "Results from " . strval($trials) . " trials with " . strval($np) . " prisoners:\n", PHP_EOL;
        foreach (["random", "optimal"] as $strat) {
            doTrials($trials, $np, $strat);
        }
    }
}
main();
?>
