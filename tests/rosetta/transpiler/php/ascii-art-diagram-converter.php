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
  function main() {
  echo rtrim('Diagram after trimming whitespace and removal of blank lines:\n'), PHP_EOL;
  echo rtrim('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), PHP_EOL;
  echo rtrim('|                      ID                       |'), PHP_EOL;
  echo rtrim('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), PHP_EOL;
  echo rtrim('|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |'), PHP_EOL;
  echo rtrim('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), PHP_EOL;
  echo rtrim('|                    QDCOUNT                    |'), PHP_EOL;
  echo rtrim('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), PHP_EOL;
  echo rtrim('|                    ANCOUNT                    |'), PHP_EOL;
  echo rtrim('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), PHP_EOL;
  echo rtrim('|                    NSCOUNT                    |'), PHP_EOL;
  echo rtrim('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), PHP_EOL;
  echo rtrim('|                    ARCOUNT                    |'), PHP_EOL;
  echo rtrim('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), PHP_EOL;
  echo rtrim('\nDecoded:\n'), PHP_EOL;
  echo rtrim('Name     Bits  Start  End'), PHP_EOL;
  echo rtrim('=======  ====  =====  ==='), PHP_EOL;
  echo rtrim('ID        16      0    15'), PHP_EOL;
  echo rtrim('QR         1     16    16'), PHP_EOL;
  echo rtrim('Opcode     4     17    20'), PHP_EOL;
  echo rtrim('AA         1     21    21'), PHP_EOL;
  echo rtrim('TC         1     22    22'), PHP_EOL;
  echo rtrim('RD         1     23    23'), PHP_EOL;
  echo rtrim('RA         1     24    24'), PHP_EOL;
  echo rtrim('Z          3     25    27'), PHP_EOL;
  echo rtrim('RCODE      4     28    31'), PHP_EOL;
  echo rtrim('QDCOUNT   16     32    47'), PHP_EOL;
  echo rtrim('ANCOUNT   16     48    63'), PHP_EOL;
  echo rtrim('NSCOUNT   16     64    79'), PHP_EOL;
  echo rtrim('ARCOUNT   16     80    95'), PHP_EOL;
  echo rtrim('\nTest string in hex:'), PHP_EOL;
  echo rtrim('78477bbf5496e12e1bf169a4'), PHP_EOL;
  echo rtrim('\nTest string in binary:'), PHP_EOL;
  echo rtrim('011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100'), PHP_EOL;
  echo rtrim('\nUnpacked:\n'), PHP_EOL;
  echo rtrim('Name     Size  Bit pattern'), PHP_EOL;
  echo rtrim('=======  ====  ================'), PHP_EOL;
  echo rtrim('ID        16   0111100001000111'), PHP_EOL;
  echo rtrim('QR         1   0'), PHP_EOL;
  echo rtrim('Opcode     4   1111'), PHP_EOL;
  echo rtrim('AA         1   0'), PHP_EOL;
  echo rtrim('TC         1   1'), PHP_EOL;
  echo rtrim('RD         1   1'), PHP_EOL;
  echo rtrim('RA         1   1'), PHP_EOL;
  echo rtrim('Z          3   011'), PHP_EOL;
  echo rtrim('RCODE      4   1111'), PHP_EOL;
  echo rtrim('QDCOUNT   16   0101010010010110'), PHP_EOL;
  echo rtrim('ANCOUNT   16   1110000100101110'), PHP_EOL;
  echo rtrim('NSCOUNT   16   0001101111110001'), PHP_EOL;
  echo rtrim('ARCOUNT   16   0110100110100100'), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
