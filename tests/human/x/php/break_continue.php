<?php
$numbers = [1,2,3,4,5,6,7,8,9];
foreach ($numbers as $n) {
    if ($n % 2 == 0) {
        continue;
    }
    if ($n > 7) {
        break;
    }
    var_dump("odd number:", $n);
}
?>
