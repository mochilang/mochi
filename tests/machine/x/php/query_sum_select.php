<?php
$nums = [1, 2, 3];
$result = (function() use ($nums) {
    $result = 0;
    foreach ($nums as $n) {
        if ($n > 1) {
            $result += $n;
        }
    }
    return $result;
})();
echo $result, PHP_EOL;
?>
