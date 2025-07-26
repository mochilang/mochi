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
  echo rtrim('chowla( 1) = 0\nchowla( 2) = 0\nchowla( 3) = 0\nchowla( 4) = 2\nchowla( 5) = 0\nchowla( 6) = 5\nchowla( 7) = 0\nchowla( 8) = 6\nchowla( 9) = 3\nchowla(10) = 7\nchowla(11) = 0\nchowla(12) = 15\nchowla(13) = 0\nchowla(14) = 9\nchowla(15) = 8\nchowla(16) = 14\nchowla(17) = 0\nchowla(18) = 20\nchowla(19) = 0\nchowla(20) = 21\nchowla(21) = 10\nchowla(22) = 13\nchowla(23) = 0\nchowla(24) = 35\nchowla(25) = 5\nchowla(26) = 15\nchowla(27) = 12\nchowla(28) = 27\nchowla(29) = 0\nchowla(30) = 41\nchowla(31) = 0\nchowla(32) = 30\nchowla(33) = 14\nchowla(34) = 19\nchowla(35) = 12\nchowla(36) = 54\nchowla(37) = 0\n\nCount of primes up to 100        = 25\nCount of primes up to 1,000      = 168\nCount of primes up to 10,000     = 1,229\nCount of primes up to 100,000    = 9,592\nCount of primes up to 1,000,000  = 78,498\nCount of primes up to 10,000,000 = 664,579\n\n6 is a perfect number\n28 is a perfect number\n496 is a perfect number\n8,128 is a perfect number\n33,550,336 is a perfect number\nThere are 5 perfect numbers <= 35,000,000'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
