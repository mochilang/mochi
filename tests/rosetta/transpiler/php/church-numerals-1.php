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
  function zero($f) {
  global $z, $three, $four;
  return function($x) use ($f) {
  return $x;
};
};
  function succ($c) {
  global $z, $three, $four;
  return function($f) use ($c) {
  return function($x) use ($c, $f, $call_user_func) {
  return $f(call_user_func($c($f), $x));
};
};
};
  function add($c, $d) {
  global $z, $three, $four;
  return function($f) use ($c, $d) {
  return function($x) use ($c, $d, $f, $call_user_func) {
  return call_user_func($c($f), call_user_func($d($f), $x));
};
};
};
  function mul($c, $d) {
  global $z, $three, $four;
  return function($f) use ($d, $c) {
  return function($x) use ($c, $d, $f, $call_user_func) {
  return call_user_func($c($d($f)), $x);
};
};
};
  function mochi_pow($c, $d) {
  global $z, $three, $four;
  $di = toInt($d);
  $prod = $c;
  $i = 1;
  while ($i < $di) {
  $prod = mul($prod, $c);
  $i = $i + 1;
};
  return $prod;
};
  function incr($i) {
  global $z, $three, $four;
  return (intval($i)) + 1;
};
  function toInt($c) {
  global $z, $three, $four;
  return intval(call_user_func($c('incr'), 0));
};
  function intToChurch($i) {
  global $z, $three, $four;
  if ($i == 0) {
  return 'zero';
}
  return succ(intToChurch($i - 1));
};
  $z = 'zero';
  $three = succ(succ(succ($z)));
  $four = succ($three);
  echo rtrim('three        -> ' . _str(toInt($three))), PHP_EOL;
  echo rtrim('four         -> ' . _str(toInt($four))), PHP_EOL;
  echo rtrim('three + four -> ' . _str(toInt(add($three, $four)))), PHP_EOL;
  echo rtrim('three * four -> ' . _str(toInt(mul($three, $four)))), PHP_EOL;
  echo rtrim('three ^ four -> ' . _str(toInt(mochi_pow($three, $four)))), PHP_EOL;
  echo rtrim('four ^ three -> ' . _str(toInt(mochi_pow($four, $three)))), PHP_EOL;
  echo rtrim('5 -> five    -> ' . _str(toInt(intToChurch(5)))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
