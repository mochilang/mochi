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
  function mochi_shuffle($xs) {
  $arr = $xs;
  $i = 99;
  while ($i > 0) {
  $j = _now() % ($i + 1);
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
  $i = $i - 1;
};
  return $arr;
};
  function doTrials($trials, $np, $strategy) {
  $pardoned = 0;
  $t = 0;
  while ($t < $trials) {
  $drawers = [];
  $i = 0;
  while ($i < 100) {
  $drawers = array_merge($drawers, [$i]);
  $i = $i + 1;
};
  $drawers = mochi_shuffle($drawers);
  $p = 0;
  $success = true;
  while ($p < $np) {
  $found = false;
  if ($strategy == 'optimal') {
  $prev = $p;
  $d = 0;
  while ($d < 50) {
  $mochi_this = $drawers[$prev];
  if ($mochi_this == $p) {
  $found = true;
  break;
}
  $prev = $mochi_this;
  $d = $d + 1;
};
} else {
  $opened = [];
  $k = 0;
  while ($k < 100) {
  $opened = array_merge($opened, [false]);
  $k = $k + 1;
};
  $d = 0;
  while ($d < 50) {
  $n = _now() % 100;
  while ($opened[$n]) {
  $n = _now() % 100;
};
  $opened[$n] = true;
  if ($drawers[$n] == $p) {
  $found = true;
  break;
}
  $d = $d + 1;
};
}
  if (!$found) {
  $success = false;
  break;
}
  $p = $p + 1;
};
  if ($success) {
  $pardoned = $pardoned + 1;
}
  $t = $t + 1;
};
  $rf = (floatval($pardoned)) / (floatval($trials)) * 100.0;
  echo rtrim('  strategy = ' . $strategy . '  pardoned = ' . _str($pardoned) . ' relative frequency = ' . _str($rf) . '%'), PHP_EOL;
};
  function main() {
  $trials = 1000;
  foreach ([10, 100] as $np) {
  echo rtrim('Results from ' . _str($trials) . ' trials with ' . _str($np) . ' prisoners:
'), PHP_EOL;
  foreach (['random', 'optimal'] as $strat) {
  doTrials($trials, $np, $strat);
};
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
