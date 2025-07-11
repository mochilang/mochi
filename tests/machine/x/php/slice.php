<?php
var_dump(array_slice([
    1,
    2,
    3
], 1, 3 - 1));
var_dump(array_slice([
    1,
    2,
    3
], 0, 2 - 0));
var_dump(substr("hello", 1, 4 - 1));
?>
