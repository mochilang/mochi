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
  function digitSumMod($n, $base) {
  $sum = 0;
  $j = $n;
  while ($j > 0) {
  $sum = $sum + $j % $base;
  $j = _intdiv($j, $base);
};
  return $sum % $base;
};
  function fairshareList($n, $base) {
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = array_merge($res, [digitSumMod($i, $base)]);
  $i = $i + 1;
};
  return $res;
};
  function sortInts($xs) {
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = 0;
  while ($j < count($arr) - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $t = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $t;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function turns($n, $base) {
  $counts = [];
  $i = 0;
  while ($i < $base) {
  $counts = array_merge($counts, [0]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $v = digitSumMod($i, $base);
  $counts[$v] = $counts[$v] + 1;
  $i = $i + 1;
};
  $freq = [];
  $fkeys = [];
  $i = 0;
  while ($i < $base) {
  $c = $counts[$i];
  if ($c > 0) {
  if (array_key_exists($c, $freq)) {
  $freq[$c] = $freq[$c] + 1;
} else {
  $freq[$c] = 1;
  $fkeys = array_merge($fkeys, [$c]);
};
}
  $i = $i + 1;
};
  $total = 0;
  $i = 0;
  while ($i < count($fkeys)) {
  $total = $total + $freq[$fkeys[$i]];
  $i = $i + 1;
};
  if ($total != $base) {
  return 'only ' . _str($total) . ' have a turn';
}
  $fkeys = sortInts($fkeys);
  $res = '';
  $i = 0;
  while ($i < count($fkeys)) {
  if ($i > 0) {
  $res = $res . ' or ';
}
  $res = $res . _str($fkeys[$i]);
  $i = $i + 1;
};
  return $res;
};
  function main() {
  $bases1 = [2, 3, 5, 11];
  $i = 0;
  while ($i < count($bases1)) {
  $b = $bases1[$i];
  echo rtrim(str_pad(_str($b), 2, ' ', STR_PAD_LEFT) . ' : ' . _str(fairshareList(25, $b))), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('How many times does each get a turn in 50000 iterations?'), PHP_EOL;
  $bases2 = [191, 1377, 49999, 50000, 50001];
  $i = 0;
  while ($i < count($bases2)) {
  $b = $bases2[$i];
  $t = turns(50000, $b);
  echo rtrim('  With ' . _str($b) . ' people: ' . $t), PHP_EOL;
  $i = $i + 1;
};
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
