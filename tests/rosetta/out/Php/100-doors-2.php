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
    echo $line, "\n";
}
?>
