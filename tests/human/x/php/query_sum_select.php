<?php
$nums = [1,2,3];
$result = 0;
foreach ($nums as $n) {
    if ($n > 1) $result += $n;
}
var_dump($result);
?>
