<?php
function countRangeSum($nums, $lower, $upper) {
    $pref = [0];
    foreach ($nums as $x) $pref[] = end($pref) + $x;
    $sort = function($lo, $hi) use (&$sort, &$pref, $lower, $upper) {
        if ($hi - $lo <= 1) return 0;
        $mid = intdiv($lo + $hi, 2);
        $ans = $sort($lo, $mid) + $sort($mid, $hi);
        $left = $lo; $right = $lo;
        for ($r = $mid; $r < $hi; $r++) {
            while ($left < $mid && $pref[$left] < $pref[$r] - $upper) $left++;
            while ($right < $mid && $pref[$right] <= $pref[$r] - $lower) $right++;
            $ans += $right - $left;
        }
        $slice = array_slice($pref, $lo, $hi - $lo);
        sort($slice, SORT_REGULAR);
        array_splice($pref, $lo, $hi - $lo, $slice);
        return $ans;
    };
    return $sort(0, count($pref));
}
$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (count($data) && $data[0] !== '') {
    $idx = 0; $t = intval($data[$idx++]); $out = [];
    for ($tc = 0; $tc < $t; $tc++) {
        $n = intval($data[$idx++]); $nums = [];
        for ($i = 0; $i < $n; $i++) $nums[] = intval($data[$idx++]);
        $lower = intval($data[$idx++]); $upper = intval($data[$idx++]);
        $out[] = strval(countRangeSum($nums, $lower, $upper));
    }
    echo implode("\n\n", $out);
}
?>
