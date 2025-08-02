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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function hailstone($n) {
  $seq = [];
  $x = $n;
  $seq = array_merge($seq, [$x]);
  while ($x > 1) {
  if ($x % 2 == 0) {
  $x = _intdiv($x, 2);
} else {
  $x = 3 * $x + 1;
}
  $seq = array_merge($seq, [$x]);
};
  return $seq;
};
  function listString($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function libMain() {
  $seq = hailstone(27);
  echo rtrim(''), PHP_EOL;
  echo rtrim('Hailstone sequence for the number 27:'), PHP_EOL;
  echo rtrim('  has ' . _str(count($seq)) . ' elements'), PHP_EOL;
  echo rtrim('  starts with ' . listString(array_slice($seq, 0, 4 - 0))), PHP_EOL;
  echo rtrim('  ends with ' . listString(array_slice($seq, count($seq) - 4, count($seq) - (count($seq) - 4)))), PHP_EOL;
  $longest = 0;
  $length = 0;
  $i = 1;
  while ($i < 100000) {
  $l = count(hailstone($i));
  if ($l > $length) {
  $longest = $i;
  $length = $l;
}
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
  echo rtrim(_str($longest) . ' has the longest Hailstone sequence, its length being ' . _str($length) . '.'), PHP_EOL;
};
  libMain();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
