<?php

function mochi_fourSum($nums, $target)
{
    $sorted = (function () use ($nums) {
        $_src = $nums;
        return _query($_src, [
        ], [ 'select' => function ($n) use ($nums) {
            return $n;
        }, 'sortKey' => function ($n) use ($nums) {
            return ($n);
        } ]);
    })();
    $n = (is_array($sorted) ? count($sorted) : strlen($sorted));
    $result = [];
    for ($i = 0; $i < $n; $i++) {
        if ((($i > 0) && ($sorted[$i] == $sorted[($i - 1)]))) {
            continue;
        }
        for ($j = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1))); $j < $n; $j++) {
            if ((($j > ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))) && ($sorted[$j] == $sorted[($j - 1)]))) {
                continue;
            }
            $left = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
            $right = ($n - 1);
            while (($left < $right)) {
                $sum = ((is_array(((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left])))) && is_array($sorted[$right])) ? array_merge(((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left]))), $sorted[$right]) : ((is_string(((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left])))) || is_string($sorted[$right])) ? (((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left]))) . $sorted[$right]) : (((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left]))) + $sorted[$right])));
                if (($sum == $target)) {
                    $result = ((is_array($result) && is_array([[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]])) ? array_merge($result, [[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]]) : ((is_string($result) || is_string([[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]])) ? ($result . [[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]]) : ($result + [[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]])));
                    $left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
                    $right = ($right - 1);
                    while ((($left < $right) && ($sorted[$left] == $sorted[($left - 1)]))) {
                        $left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
                    }
                    while ((($left < $right) && ($sorted[$right] == $sorted[((is_array($right) && is_array(1)) ? array_merge($right, 1) : ((is_string($right) || is_string(1)) ? ($right . 1) : ($right + 1)))]))) {
                        $right = ($right - 1);
                    }
                } elseif (($sum < $target)) {
                    $left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
                } else {
                    $right = ($right - 1);
                }
            }
        }
    }
    return $result;
}

function mochi_test_example_1()
{
    if (!((mochi_fourSum([1, 0, -1, 0, -2, 2], 0) == [[-2, -1, 1, 2], [-2, 0, 0, 2], [-1, 0, 0, 1]]))) {
        throw new Exception('expect failed');
    }
}

function mochi_test_example_2()
{
    if (!((mochi_fourSum([2, 2, 2, 2, 2], 8) == [[2, 2, 2, 2]]))) {
        throw new Exception('expect failed');
    }
}

mochi_test_example_1();
mochi_test_example_2();

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
