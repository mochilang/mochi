<?php
$result = "";
for ($i = 1; $i < 101; $i++) {
    $j = 1;
    while ($j * $j < $i) {
        $j = $j + 1;
    }
    if ($j * $j == $i) {
        $result = $result . "O";
    } else {
        $result = $result . "-";
    }
}
echo $result, "\n";
?>
