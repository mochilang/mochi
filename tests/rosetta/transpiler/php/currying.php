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
  function Foo_Method($self, $b) {
  $value = $self['value'];
  return $value + $b;
};
  function mochi_pow($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < intval($exp)) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function PowN($b) {
  return function($e) use ($b) {
  return mochi_pow($b, $e);
};
};
  function PowE($e) {
  return function($b) use ($e) {
  return mochi_pow($b, $e);
};
};
  function main() {
  $pow2 = PowN(2.0);
  $cube = PowE(3.0);
  echo rtrim('2^8 = ' . _str($pow2(8.0))), PHP_EOL;
  echo rtrim('4³ = ' . _str($cube(4.0))), PHP_EOL;
  $a = ['value' => 2];
  $fn1 = null;
$fn1 = function($b) use ($a, $fn1, $pow2, $cube) {
  return Foo_Method($a, $b);
};
  $fn2 = null;
$fn2 = function($f, $b) use ($fn1, $fn2, $pow2, $cube, $a) {
  return Foo_Method($f, $b);
};
  echo rtrim('2 + 2 = ' . _str(Foo_Method($a, 2))), PHP_EOL;
  echo rtrim('2 + 3 = ' . _str($fn1(3))), PHP_EOL;
  echo rtrim('2 + 4 = ' . _str($fn2($a, 4))), PHP_EOL;
  echo rtrim('3 + 5 = ' . _str($fn2(['value' => 3], 5))), PHP_EOL;
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
