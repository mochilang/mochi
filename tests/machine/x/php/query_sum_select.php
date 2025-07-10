<?php
$nums = [1, 2, 3];
$result = (function() use ($nums) {
    $result = [];
    foreach ($nums as $n) {
        if ($n > 1) {
            $result[] = array_sum($n);
        }
    }
    return $result;
})();
var_dump($result);
