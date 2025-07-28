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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
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
  $res = $testpkg['ECDSAExample']();
  echo rtrim('Private key:
D: ' . $res['D']), PHP_EOL;
  echo rtrim('
Public key:'), PHP_EOL;
  echo rtrim('X: ' . $res['X']), PHP_EOL;
  echo rtrim('Y: ' . $res['Y']), PHP_EOL;
  echo rtrim('
Message: Rosetta Code'), PHP_EOL;
  echo rtrim('Hash   : ' . $res['Hash']), PHP_EOL;
  echo rtrim('
Signature:'), PHP_EOL;
  echo rtrim('R: ' . $res['R']), PHP_EOL;
  echo rtrim('S: ' . $res['S']), PHP_EOL;
  echo rtrim('
Signature verified: ' . _str($res['Valid'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
