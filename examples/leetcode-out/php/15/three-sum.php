<?php

function mochi_threeSum($nums)
{
    $sorted = (function () use ($nums) {
        $_src = $nums;
        return _query($_src, [
        ], [ 'select' => function ($x) use ($nums) {
            return $x;
        }, 'sortKey' => function ($x) use ($nums) {
            return ($x);
        } ]);
    })();
    $n = (is_array($sorted) ? count($sorted) : strlen($sorted));
    $res = [];
    $i = 0;
    while (($i < $n)) {
        if ((($i > 0) && ($sorted[$i] == $sorted[($i - 1)]))) {
            $i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
            continue;
        }
        $left = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
        $right = ($n - 1);
        while (($left < $right)) {
            $sum = ((is_array(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left])))) && is_array($sorted[$right])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))), $sorted[$right]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left])))) || is_string($sorted[$right])) ? (((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))) . $sorted[$right]) : (((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))) + $sorted[$right])));
            if (($sum == 0)) {
                $res = ((is_array($res) && is_array([[$sorted[$i], $sorted[$left], $sorted[$right]]])) ? array_merge($res, [[$sorted[$i], $sorted[$left], $sorted[$right]]]) : ((is_string($res) || is_string([[$sorted[$i], $sorted[$left], $sorted[$right]]])) ? ($res . [[$sorted[$i], $sorted[$left], $sorted[$right]]]) : ($res + [[$sorted[$i], $sorted[$left], $sorted[$right]]])));
                $left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
                while ((($left < $right) && ($sorted[$left] == $sorted[($left - 1)]))) {
                    $left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
                }
                $right = ($right - 1);
                while ((($left < $right) && ($sorted[$right] == $sorted[((is_array($right) && is_array(1)) ? array_merge($right, 1) : ((is_string($right) || is_string(1)) ? ($right . 1) : ($right + 1)))]))) {
                    $right = ($right - 1);
                }
            } elseif (($sum < 0)) {
                $left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
            } else {
                $right = ($right - 1);
            }
        }
        $i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
    }
    return $res;
}

function mochi_test_example_1()
{
    if (!((mochi_threeSum([-1, 0, 1, 2, -1, -4]) == [[-1, -1, 2], [-1, 0, 1]]))) {
        throw new Exception('expect failed');
    }
}

function mochi_test_example_2()
{
    if (!((mochi_threeSum([0, 1, 1]) == []))) {
        throw new Exception('expect failed');
    }
}

function mochi_test_example_3()
{
    if (!((mochi_threeSum([0, 0, 0]) == [[0, 0, 0]]))) {
        throw new Exception('expect failed');
    }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();

function _query($src, $joins, $opts)
{
    $items = array_map(fn($v) => [$v], $src);
    foreach ($joins as $j) {
        $joined = [];
        if (!empty($j['right']) && !empty($j['left'])) {
            $matched = array_fill(0, count($j['items']), false);
            foreach ($items as $left) {
                $m = false;
                foreach ($j['items'] as $ri => $right) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = array_merge($left, [$right]);
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) {
                        continue;
                    }
                    $m = true;
                    $matched[$ri] = true;
                    $joined[] = array_merge($left, [$right]);
                }
                if (!$m) {
                    $joined[] = array_merge($left, [null]);
                }
            }
            foreach ($j['items'] as $ri => $right) {
                if (!$matched[$ri]) {
                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];
                    $joined[] = array_merge($undef, [$right]);
                }
            }
        } elseif (!empty($j['right'])) {
            foreach ($j['items'] as $right) {
                $m = false;
                foreach ($items as $left) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = array_merge($left, [$right]);
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) {
                        continue;
                    }
                    $m = true;
                    $joined[] = array_merge($left, [$right]);
                }
                if (!$m) {
                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];
                    $joined[] = array_merge($undef, [$right]);
                }
            }
        } else {
            foreach ($items as $left) {
                $m = false;
                foreach ($j['items'] as $right) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = array_merge($left, [$right]);
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) {
                        continue;
                    }
                    $m = true;
                    $joined[] = array_merge($left, [$right]);
                }
                if (!empty($j['left']) && !$m) {
                    $joined[] = array_merge($left, [null]);
                }
            }
        }
        $items = $joined;
    }
    if (isset($opts['where'])) {
        $filtered = [];
        foreach ($items as $r) {
            if ($opts['where'](...$r)) {
                $filtered[] = $r;
            }
        }
        $items = $filtered;
    }
    if (isset($opts['sortKey'])) {
        $pairs = [];
        foreach ($items as $it) {
            $pairs[] = ['item' => $it, 'key' => $opts['sortKey'](...$it)];
        }
        usort($pairs, function ($a, $b) {
            $ak = $a['key'];
            $bk = $b['key'];
            if (is_int($ak) && is_int($bk)) {
                return $ak <=> $bk;
            }
            if (is_string($ak) && is_string($bk)) {
                return $ak <=> $bk;
            }
            return strcmp(strval($ak), strval($bk));
        });
        $items = array_map(fn($p) => $p['item'], $pairs);
    }
    if (array_key_exists('skip', $opts)) {
        $n = $opts['skip'];
        $items = $n < count($items) ? array_slice($items, $n) : [];
    }
    if (array_key_exists('take', $opts)) {
        $n = $opts['take'];
        if ($n < count($items)) {
            $items = array_slice($items, 0, $n);
        }
    }
    $res = [];
    foreach ($items as $r) {
        $res[] = $opts['select'](...$r);
    }
    return $res;
}
