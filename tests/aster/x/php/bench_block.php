<?php
ini_set("memory_limit", "-1");
$now_seed = 0;
;
$s = getenv("MOCHI_NOW_SEED");
if ( && $s !== "''") {
    $now_seed = intval($s);
    ;
}
$__start_mem = ;
$__start = ;
$n = 1000;
$s = 0;
for ($i = 1; $i < $n; $i++) {
    $s = $s + $i;
}
$__end = ;
$__end_mem = ;
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = 0;
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "simple"];
$__j = json_encode($__bench, 128);
$__j = str_replace($__j);
echo $__j, PHP_EOL;
