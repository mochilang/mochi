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
  function powInt($base, $exp) {
  $r = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $r = $r * $b;
}
  $b = $b * $b;
  $e = $e / intval(2);
};
  return $r;
};
  function minInt($x, $y) {
  if ($x < $y) {
  return $x;
}
  return $y;
};
  function throwDie($nSides, $nDice, $s, &$counts) {
  if ($nDice == 0) {
  $counts[$s] = $counts[$s] + 1;
  return;
}
  $i = 1;
  while ($i <= $nSides) {
  throwDie($nSides, $nDice - 1, $s + $i, $counts);
  $i = $i + 1;
};
};
  function beatingProbability($nSides1, $nDice1, $nSides2, $nDice2) {
  $len1 = ($nSides1 + 1) * $nDice1;
  $c1 = [];
  $i = 0;
  while ($i < $len1) {
  $c1 = array_merge($c1, [0]);
  $i = $i + 1;
};
  throwDie($nSides1, $nDice1, 0, $c1);
  $len2 = ($nSides2 + 1) * $nDice2;
  $c2 = [];
  $j = 0;
  while ($j < $len2) {
  $c2 = array_merge($c2, [0]);
  $j = $j + 1;
};
  throwDie($nSides2, $nDice2, 0, $c2);
  $p12 = (floatval(powInt($nSides1, $nDice1))) * (floatval(powInt($nSides2, $nDice2)));
  $tot = 0.0;
  $i = 0;
  while ($i < $len1) {
  $j = 0;
  $m = minInt($i, $len2);
  while ($j < $m) {
  $tot = $tot + ($c1[$i] * floatval($c2[$j])) / $p12;
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $tot;
};
  echo rtrim(_str(beatingProbability(4, 9, 6, 6))), PHP_EOL;
  echo rtrim(_str(beatingProbability(10, 5, 7, 6))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
