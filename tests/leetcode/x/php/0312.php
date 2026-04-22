<?php
function maxCoins(array $nums): int {
    $vals = array_merge([1], $nums, [1]);
    $n = count($vals);
    $dp = array_fill(0, $n, array_fill(0, $n, 0));
    for ($length = 2; $length < $n; $length++) {
        for ($left = 0; $left + $length < $n; $left++) {
            $right = $left + $length;
            for ($k = $left + 1; $k < $right; $k++) {
                $dp[$left][$right] = max($dp[$left][$right], $dp[$left][$k] + $dp[$k][$right] + $vals[$left] * $vals[$k] * $vals[$right]);
            }
        }
    }
    return $dp[0][$n - 1];
}

$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$data || $data[0] === '') exit(0);
$idx = 0;
$t = intval($data[$idx++]);
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($data[$idx++]);
    $nums = [];
    for ($i = 0; $i < $n; $i++) $nums[] = intval($data[$idx++]);
    $blocks[] = strval(maxCoins($nums));
}
echo implode("\n\n", $blocks);
