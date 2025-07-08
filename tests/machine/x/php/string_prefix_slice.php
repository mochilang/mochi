<?php
$prefix = "fore";
$s1 = "forest";
var_dump(substr($s1, 0, strlen($prefix) - 0) == $prefix);
$s2 = "desert";
var_dump(substr($s2, 0, strlen($prefix) - 0) == $prefix);
