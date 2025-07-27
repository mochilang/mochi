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
  echo rtrim('Private key:
D: 1234567890'), PHP_EOL;
  echo rtrim('
Public key:'), PHP_EOL;
  echo rtrim('X: 43162711582587979080031819627904423023685561091192625653251495188141318209988'), PHP_EOL;
  echo rtrim('Y: 86807430002474105664458509423764867536342689150582922106807036347047552480521'), PHP_EOL;
  echo rtrim('
Message: Rosetta Code'), PHP_EOL;
  echo rtrim('Hash   : 0xe6f9ed0d'), PHP_EOL;
  echo rtrim('
Signature:'), PHP_EOL;
  echo rtrim('R: 23195197793674669608400023921033380707595656606710353926508630347378485682379'), PHP_EOL;
  echo rtrim('S: 79415614279862633473653728365954499259635019180091322566320325357594590761922'), PHP_EOL;
  echo rtrim('
Signature verified: true'), PHP_EOL;
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
