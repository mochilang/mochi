<?php
$data = [1, 2];
$flag = count((function() use ($data) {
    $result = [];
    foreach ($data as $x) {
        if ($x == 1) {
            $result[] = $x;
        }
    }
    return $result;
})()) > 0;
var_dump($flag);
?>
