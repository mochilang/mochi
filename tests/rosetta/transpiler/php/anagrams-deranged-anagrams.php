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
  function sortRunes($s) {
  $arr = [];
  $i = 0;
  while ($i < strlen($s)) {
  $arr = array_merge($arr, [substr($s, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $n = count($arr);
  $m = 0;
  while ($m < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $m = $m + 1;
};
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return $out;
};
  function deranged($a, $b) {
  if (strlen($a) != strlen($b)) {
  return false;
}
  $i = 0;
  while ($i < strlen($a)) {
  if (substr($a, $i, $i + 1 - $i) == substr($b, $i, $i + 1 - $i)) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function main() {
  $words = ['constitutionalism', 'misconstitutional'];
  $m = [];
  $bestLen = 0;
  $w1 = '';
  $w2 = '';
  foreach ($words as $w) {
  if (strlen($w) <= $bestLen) {
  continue;
}
  $k = sortRunes($w);
  if (!(array_key_exists($k, $m))) {
  $m[$k] = [$w];
  continue;
}
  foreach ($m[$k] as $c) {
  if (deranged($w, $c)) {
  $bestLen = strlen($w);
  $w1 = $c;
  $w2 = $w;
  break;
}
};
  $m[$k] = array_merge($m[$k], [$w]);
};
  echo rtrim($w1 . ' ' . $w2 . ' : Length ' . _str($bestLen)), PHP_EOL;
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
