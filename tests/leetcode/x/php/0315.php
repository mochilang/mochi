<?php
function sortCounts(array $nums, array &$idx, array &$tmp, array &$counts, int $lo, int $hi): void {
    if ($hi - $lo <= 1) return;
    $mid = intdiv($lo + $hi, 2);
    sortCounts($nums, $idx, $tmp, $counts, $lo, $mid);
    sortCounts($nums, $idx, $tmp, $counts, $mid, $hi);
    $i = $lo; $j = $mid; $k = $lo; $moved = 0;
    while ($i < $mid && $j < $hi) {
        if ($nums[$idx[$j]] < $nums[$idx[$i]]) {
            $tmp[$k++] = $idx[$j++];
            $moved++;
        } else {
            $counts[$idx[$i]] += $moved;
            $tmp[$k++] = $idx[$i++];
        }
    }
    while ($i < $mid) {
        $counts[$idx[$i]] += $moved;
        $tmp[$k++] = $idx[$i++];
    }
    while ($j < $hi) $tmp[$k++] = $idx[$j++];
    for ($p = $lo; $p < $hi; $p++) $idx[$p] = $tmp[$p];
}

function countSmaller(array $nums): array {
    $n = count($nums);
    $counts = array_fill(0, $n, 0);
    $idx = range(0, $n - 1);
    $tmp = array_fill(0, $n, 0);
    sortCounts($nums, $idx, $tmp, $counts, 0, $n);
    return $counts;
}

$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$data || $data[0] === '') exit(0);
$pos = 0;
$t = intval($data[$pos++]);
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($data[$pos++]);
    $nums = [];
    for ($i = 0; $i < $n; $i++) $nums[] = intval($data[$pos++]);
    $blocks[] = '[' . implode(',', countSmaller($nums)) . ']';
}
echo implode("\n\n", $blocks);
