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
  $width = 320;
  $height = 240;
  $seed = fmod(_now(), 2147483647);
  function nextRand() {
  global $width, $height, $seed, $y, $line, $x, $val;
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  return $seed;
};
  function randBit() {
  global $width, $height, $seed, $y, $line, $x, $val;
  $r = nextRand();
  if ($r % 2 == 0) {
  return 0;
}
  return 255;
};
  echo rtrim('P2'), PHP_EOL;
  echo rtrim(_str($width) . ' ' . _str($height)), PHP_EOL;
  echo rtrim('255'), PHP_EOL;
  $y = 0;
  while ($y < $height) {
  $line = '';
  $x = 0;
  while ($x < $width) {
  $val = randBit();
  $line = $line . _str($val);
  if ($x < $width - 1) {
  $line = $line . ' ';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
