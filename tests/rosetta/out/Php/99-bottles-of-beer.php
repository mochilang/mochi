<?php
function bottles($n) {
    if ($n == 0) {
        return "No more bottles";
    }
    if ($n == 1) {
        return "1 bottle";
    }
    return strval($n) . " bottles";
}
function main() {
    $i = 99;
    while ($i > 0) {
        var_dump(bottles($i) . " of beer on the wall");
        var_dump(bottles($i) . " of beer");
        echo "Take one down, pass it around", PHP_EOL;
        var_dump(bottles($i - 1) . " of beer on the wall");
        $i = $i - 1;
    }
}
main();
?>
