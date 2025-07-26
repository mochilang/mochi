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
  function id($x) {
  return $x;
};
  function compose($f, $g) {
  return function($x) use ($f, $g) {
  return $f($g($x));
};
};
  function zero() {
  return function($f) {
  return 'id';
};
};
  function one() {
  return 'id';
};
  function succ($n) {
  return function($f) use ($n) {
  return compose($f, $n($f));
};
};
  function plus($m, $n) {
  return function($f) use ($m, $n) {
  return compose($m($f), $n($f));
};
};
  function mult($m, $n) {
  return compose($m, $n);
};
  function mochi_exp($m, $n) {
  return $n($m);
};
  function toInt($x) {
  $counter = 0;
  $fCounter = function($f) use (&$fCounter, $x, $counter) {
  $counter = $counter + 1;
  return $f;
};
  call_user_func($x($fCounter), 'id');
  return $counter;
};
  function toStr($x) {
  $s = '';
  $fCounter = function($f) use (&$fCounter, $x, $s) {
  $s = $s . '|';
  return $f;
};
  call_user_func($x($fCounter), 'id');
  return $s;
};
  function main() {
  echo rtrim('zero = ' . _str(toInt(zero()))), PHP_EOL;
  $onev = one();
  echo rtrim('one = ' . _str(toInt($onev))), PHP_EOL;
  $two = succ(succ(zero()));
  echo rtrim('two = ' . _str(toInt($two))), PHP_EOL;
  $three = plus($onev, $two);
  echo rtrim('three = ' . _str(toInt($three))), PHP_EOL;
  $four = mult($two, $two);
  echo rtrim('four = ' . _str(toInt($four))), PHP_EOL;
  $eight = mochi_exp($two, $three);
  echo rtrim('eight = ' . _str(toInt($eight))), PHP_EOL;
  echo rtrim('toStr(four) = ' . toStr($four)), PHP_EOL;
};
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
