<?php
function sum_rec($n, $acc) {
    if ($n == 0) {
        return $acc;
    }
    return sum_rec($n - 1, $acc + $n);
}
var_dump(sum_rec(10, 0));
?>
