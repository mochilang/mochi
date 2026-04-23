<?php
function pickSeq(array $nums, int $k): array {
    $drop = count($nums) - $k;
    $stack = [];
    foreach ($nums as $x) {
        while ($drop > 0 && count($stack) > 0 && $stack[count($stack) - 1] < $x) {
            array_pop($stack);
            $drop--;
        }
        $stack[] = $x;
    }
    return array_slice($stack, 0, $k);
}

function greaterSeq(array $a, int $i, array $b, int $j): bool {
    while ($i < count($a) && $j < count($b) && $a[$i] === $b[$j]) {
        $i++;
        $j++;
    }
    return $j === count($b) || ($i < count($a) && $a[$i] > $b[$j]);
}

function mergeSeq(array $a, array $b): array {
    $out = [];
    $i = 0; $j = 0;
    while ($i < count($a) || $j < count($b)) {
        if (greaterSeq($a, $i, $b, $j)) $out[] = $a[$i++];
        else $out[] = $b[$j++];
    }
    return $out;
}

function maxNumberSeq(array $nums1, array $nums2, int $k): array {
    $best = [];
    for ($take = max(0, $k - count($nums2)); $take <= min($k, count($nums1)); $take++) {
        $cand = mergeSeq(pickSeq($nums1, $take), pickSeq($nums2, $k - $take));
        if (greaterSeq($cand, 0, $best, 0)) $best = $cand;
    }
    return $best;
}

$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$data || $data[0] === '') exit(0);
$idx = 0;
$t = intval($data[$idx++]);
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n1 = intval($data[$idx++]);
    $nums1 = array_map('intval', array_slice($data, $idx, $n1));
    $idx += $n1;
    $n2 = intval($data[$idx++]);
    $nums2 = array_map('intval', array_slice($data, $idx, $n2));
    $idx += $n2;
    $k = intval($data[$idx++]);
    $blocks[] = '[' . implode(',', maxNumberSeq($nums1, $nums2, $k)) . ']';
}
echo implode("\n\n", $blocks);
