<?php
function twoSum($nums, $target) {
    $n = count($nums);
    for ($i = 0; $i <= $n; $i++) {
        for ($j = $i + 1; $j <= $n; $j++) {
            if ($nums[$i] + $nums[$j] == $target) {
                return [$i, $j];
            }
        }
    }
    return [-1, -1];
}
$result = twoSum([2, 7, 11, 15], 9);
var_dump($result[0]);
var_dump($result[1]);
?>
