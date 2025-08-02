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
  function contains($xs, $n) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $n) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function gcd($a, $b) {
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  if ($x < 0) {
  $x = -$x;
}
  return $x;
};
  function sortInts($xs) {
  $arr = $xs;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function areSame($s, $t) {
  if (count($s) != count($t)) {
  return false;
}
  $a = sortInts($s);
  $b = sortInts($t);
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function printSlice($start, $seq) {
  $first = [];
  $i = 0;
  while ($i < 30) {
  $first = array_merge($first, [$seq[$i]]);
  $i = $i + 1;
};
  $pad = '';
  if ($start < 10) {
  $pad = ' ';
}
  echo rtrim('EKG(' . $pad . _str($start) . '): ' . _str($first)), PHP_EOL;
};
  function main() {
  $limit = 100;
  $starts = [2, 5, 7, 9, 10];
  $ekg = [];
  $s = 0;
  while ($s < count($starts)) {
  $seq = [1, $starts[$s]];
  $n = 2;
  while ($n < $limit) {
  $i = 2;
  $done = false;
  while (!$done) {
  if (!in_array($i, $seq) && gcd($seq[$n - 1], $i) > 1) {
  $seq = array_merge($seq, [$i]);
  $done = true;
}
  $i = $i + 1;
};
  $n = $n + 1;
};
  $ekg = array_merge($ekg, [$seq]);
  printSlice($starts[$s], $seq);
  $s = $s + 1;
};
  $i = 2;
  $found = false;
  while ($i < $limit) {
  if ($ekg[1][$i] == $ekg[2][$i] && areSame(array_slice($ekg[1], 0, $i - 0), array_slice($ekg[2], 0, $i - 0))) {
  echo rtrim('
EKG(5) and EKG(7) converge at term ' . _str($i + 1)), PHP_EOL;
  $found = true;
  break;
}
  $i = $i + 1;
};
  if (!$found) {
  echo rtrim('
EKG5(5) and EKG(7) do not converge within ' . _str($limit) . ' terms'), PHP_EOL;
}
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
