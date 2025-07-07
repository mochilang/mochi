<?php
$c = ["n" => 0];
function inc(&$c) { $c['n'] = $c['n'] + 1; }
inc($c);
_print($c['n']);

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
?>
