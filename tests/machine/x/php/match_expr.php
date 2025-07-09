<?php
$x = 2;
$label = (function($_t) {
    if ($_t === 1) return "one";
    if ($_t === 2) return "two";
    if ($_t === 3) return "three";
    return "unknown";
)($x);
var_dump($label);
