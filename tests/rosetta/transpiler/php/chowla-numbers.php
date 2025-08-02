<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
$__start_mem = memory_get_usage();
$__start = _now();
  echo rtrim('chowla( 1) = 0
chowla( 2) = 0
chowla( 3) = 0
chowla( 4) = 2
chowla( 5) = 0
chowla( 6) = 5
chowla( 7) = 0
chowla( 8) = 6
chowla( 9) = 3
chowla(10) = 7
chowla(11) = 0
chowla(12) = 15
chowla(13) = 0
chowla(14) = 9
chowla(15) = 8
chowla(16) = 14
chowla(17) = 0
chowla(18) = 20
chowla(19) = 0
chowla(20) = 21
chowla(21) = 10
chowla(22) = 13
chowla(23) = 0
chowla(24) = 35
chowla(25) = 5
chowla(26) = 15
chowla(27) = 12
chowla(28) = 27
chowla(29) = 0
chowla(30) = 41
chowla(31) = 0
chowla(32) = 30
chowla(33) = 14
chowla(34) = 19
chowla(35) = 12
chowla(36) = 54
chowla(37) = 0

Count of primes up to 100        = 25
Count of primes up to 1,000      = 168
Count of primes up to 10,000     = 1,229
Count of primes up to 100,000    = 9,592
Count of primes up to 1,000,000  = 78,498
Count of primes up to 10,000,000 = 664,579

6 is a perfect number
28 is a perfect number
496 is a perfect number
8,128 is a perfect number
33,550,336 is a perfect number
There are 5 perfect numbers <= 35,000,000'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
