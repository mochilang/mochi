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
  $seed = 1;
  function mochi_rand() {
  global $seed, $integers, $strings;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return _intdiv($seed, 65536);
};
  function randint($a, $b) {
  global $seed, $integers, $strings;
  $r = mochi_rand();
  return $a + $r % ($b - $a + 1);
};
  function fisher_yates_shuffle_int($data) {
  global $seed, $integers, $strings;
  $res = $data;
  $i = 0;
  while ($i < count($res)) {
  $a = randint(0, count($res) - 1);
  $b = randint(0, count($res) - 1);
  $temp = $res[$a];
  $res[$a] = $res[$b];
  $res[$b] = $temp;
  $i = $i + 1;
};
  return $res;
};
  function fisher_yates_shuffle_str($data) {
  global $seed, $integers, $strings;
  $res = $data;
  $i = 0;
  while ($i < count($res)) {
  $a = randint(0, count($res) - 1);
  $b = randint(0, count($res) - 1);
  $temp = $res[$a];
  $res[$a] = $res[$b];
  $res[$b] = $temp;
  $i = $i + 1;
};
  return $res;
};
  $integers = [0, 1, 2, 3, 4, 5, 6, 7];
  $strings = ['python', 'says', 'hello', '!'];
  echo rtrim('Fisher-Yates Shuffle:'), PHP_EOL;
  echo rtrim('List ' . _str($integers) . ' ' . _str($strings)), PHP_EOL;
  echo rtrim('FY Shuffle ' . _str(fisher_yates_shuffle_int($integers)) . ' ' . _str(fisher_yates_shuffle_str($strings))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
