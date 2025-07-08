<?php
$x = 2;
$label = match($x) {
    1 => "one",
    2 => "two",
    3 => "three",
    default => "unknown",
};
var_dump($label);
