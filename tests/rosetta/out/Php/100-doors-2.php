<?php
$door = 1;
$incrementer = 0;
for ($current = 1; $current < 101; $current++) {
    $line = "Door " . strval($current) . " ";
    if ($current == $door) {
        $line = $line . "Open";
        $incrementer = $incrementer + 1;
        $door = $door + 2 * $incrementer + 1;
    } else {
        $line = $line . "Closed";
    }
    _print($line);
}
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
