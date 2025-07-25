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
  function listStr($xs) {
  global $a, $cap_s;
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i + 1 < count($xs)) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  $a = [0, 0, 0, 0, 0];
  echo rtrim('len(a) = ' . _str(count($a))), PHP_EOL;
  echo rtrim('a = ' . listStr($a)), PHP_EOL;
  $a[0] = 3;
  echo rtrim('a = ' . listStr($a)), PHP_EOL;
  echo rtrim('a[0] = ' . _str($a[0])), PHP_EOL;
  $s = array_slice($a, 0, 4 - 0);
  $cap_s = 5;
  echo rtrim('s = ' . listStr($s)), PHP_EOL;
  echo rtrim('len(s) = ' . _str(count($s)) . '  cap(s) = ' . _str($cap_s)), PHP_EOL;
  $s = array_slice($a, 0, 5 - 0);
  echo rtrim('s = ' . listStr($s)), PHP_EOL;
  $a[0] = 22;
  $s[0] = 22;
  echo rtrim('a = ' . listStr($a)), PHP_EOL;
  echo rtrim('s = ' . listStr($s)), PHP_EOL;
  $s = array_merge($s, [4]);
  $s = array_merge($s, [5]);
  $s = array_merge($s, [6]);
  $cap_s = 10;
  echo rtrim('s = ' . listStr($s)), PHP_EOL;
  echo rtrim('len(s) = ' . _str(count($s)) . '  cap(s) = ' . _str($cap_s)), PHP_EOL;
  $a[4] = -1;
  echo rtrim('a = ' . listStr($a)), PHP_EOL;
  echo rtrim('s = ' . listStr($s)), PHP_EOL;
  $s = [];
  for ($i = 0; $i < 8; $i++) {
  $s = array_merge($s, [0]);
}
  $cap_s = 8;
  echo rtrim('s = ' . listStr($s)), PHP_EOL;
  echo rtrim('len(s) = ' . _str(count($s)) . '  cap(s) = ' . _str($cap_s)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
