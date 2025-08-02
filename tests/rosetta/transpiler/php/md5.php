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
  $testpkg = ['Add' => function($a, $b) {
  return $a + $b;
}, 'Pi' => 3.14, 'Answer' => 42, 'FifteenPuzzleExample' => function() {
  return 'Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd';
}, 'MD5Hex' => 'md5', 'ECDSAExample' => function() {
  return ['D' => '1234567890', 'X' => '43162711582587979080031819627904423023685561091192625653251495188141318209988', 'Y' => '86807430002474105664458509423764867536342689150582922106807036347047552480521', 'Hash' => '0xe6f9ed0d', 'R' => '43162711582587979080031819627904423023685561091192625653251495188141318209988', 'S' => '94150071556658883365738746782965214584303361499725266605620843043083873122499', 'Valid' => true];
}];
  foreach ([['d41d8cd98f00b204e9800998ecf8427e', ''], ['0cc175b9c0f1b6a831c399e269772661', 'a'], ['900150983cd24fb0d6963f7d28e17f72', 'abc'], ['f96b697d7cb7938d525a2f31aaf161d0', 'message digest'], ['c3fcd3d76192e4007dfb496cca67e13b', 'abcdefghijklmnopqrstuvwxyz'], ['d174ab98d277d9f5a5611c2c9f419d9f', 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'], ['57edf4a22be3c955ac49da2e2107b67a', '12345678901234567890' . '123456789012345678901234567890123456789012345678901234567890'], ['e38ca1d920c4b8b8d3946b2c72f01680', 'The quick brown fox jumped over the lazy dog\'s back']] as $pair) {
  $sum = $testpkg['MD5Hex']($pair[1]);
  if ($sum != $pair[0]) {
  echo rtrim('MD5 fail'), PHP_EOL;
  echo rtrim('  for string,') . " " . rtrim($pair[1]), PHP_EOL;
  echo rtrim('  expected:  ') . " " . rtrim($pair[0]), PHP_EOL;
  echo rtrim('  got:       ') . " " . rtrim(json_encode($sum, 1344)), PHP_EOL;
}
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
