<?php
$doors = [];
for ($i = 0; $i < 100; $i++) {
    $doors = array_merge($doors, [false]);
}
for ($pass = 1; $pass < 101; $pass++) {
    $idx = $pass - 1;
    while ($idx < 100) {
        $doors[$idx] = !$doors[$idx];
        $idx = $idx + $pass;
    }
}
for ($row = 0; $row < 10; $row++) {
    $line = "";
    for ($col = 0; $col < 10; $col++) {
        $idx = $row * 10 + $col;
        if ($doors[$idx]) {
            $line = $line . "1";
        } else {
            $line = $line . "0";
        }
        if ($col < 9) {
            $line = $line . " ";
        }
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
