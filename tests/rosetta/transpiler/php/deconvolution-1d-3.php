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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function fmt1($x) {
  $y = floatval(intval((($x * 10.0) + 0.5))) / 10.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot < 0) {
  $s = $s . '.0';
}
  return $s;
};
  function printColumnMatrix($xs) {
  if (count($xs) == 0) {
  return;
}
  echo rtrim('⎡' . fmt1($xs[0]) . '⎤'), PHP_EOL;
  $i = 1;
  while ($i < count($xs) - 1) {
  echo rtrim('⎢' . fmt1($xs[$i]) . '⎥'), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim('⎣ ' . fmt1($xs[count($xs) - 1]) . '⎦'), PHP_EOL;
};
  function deconv($g, $f) {
  $h = [];
  $n = 0;
  $hn = count($g) - count($f) + 1;
  while ($n < $hn) {
  $v = $g[$n];
  $lower = 0;
  if ($n >= count($f)) {
  $lower = $n - count($f) + 1;
}
  $i = $lower;
  while ($i < $n) {
  $v = $v - $h[$i] * $f[$n - $i];
  $i = $i + 1;
};
  $v = $v / $f[0];
  $h = _append($h, $v);
  $n = $n + 1;
};
  return $h;
};
  function main() {
  $h = [-8.0, -9.0, -3.0, -1.0, -6.0, 7.0];
  $f = [-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0];
  $g = [24.0, 75.0, 71.0, -34.0, 3.0, 22.0, -45.0, 23.0, 245.0, 25.0, 52.0, 25.0, -67.0, -96.0, 96.0, 31.0, 55.0, 36.0, 29.0, -43.0, -7.0];
  echo rtrim('deconv(g, f) ='), PHP_EOL;
  printColumnMatrix(deconv($g, $f));
  echo rtrim(''), PHP_EOL;
  echo rtrim('deconv(g, h) ='), PHP_EOL;
  printColumnMatrix(deconv($g, $h));
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
