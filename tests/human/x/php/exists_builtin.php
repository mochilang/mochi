<?php
$data = [1,2];
$flag = false;
foreach ($data as $x) {
    if ($x == 1) { $flag = true; break; }
}
var_dump($flag ? 'true' : 'false');
?>
