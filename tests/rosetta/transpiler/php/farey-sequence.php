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
  function fracStr($f) {
  return _str($f['num']) . '/' . _str($f['den']);
};
  function gen($l, $r, $n, $acc) {
  $m = ['num' => $l['num'] + $r['num'], 'den' => $l['den'] + $r['den']];
  if ($m['den'] <= $n) {
  $acc = gen($l, $m, $n, $acc);
  $acc = array_merge($acc, [$m]);
  $acc = gen($m, $r, $n, $acc);
}
  return $acc;
};
  function totient($n) {
  $tot = $n;
  $nn = $n;
  $p = 2;
  while ($p * $p <= $nn) {
  if ($nn % $p == 0) {
  while ($nn % $p == 0) {
  $nn = _intdiv($nn, $p);
};
  $tot = $tot - _intdiv($tot, $p);
}
  if ($p == 2) {
  $p = 1;
}
  $p = $p + 2;
};
  if ($nn > 1) {
  $tot = $tot - _intdiv($tot, $nn);
}
  return $tot;
};
  function main() {
  $n = 1;
  while ($n <= 11) {
  $l = ['num' => 0, 'den' => 1];
  $r = ['num' => 1, 'den' => 1];
  $seq = gen($l, $r, $n, []);
  $line = 'F(' . _str($n) . '): ' . fracStr($l);
  foreach ($seq as $f) {
  $line = $line . ' ' . fracStr($f);
};
  $line = $line . ' ' . fracStr($r);
  echo rtrim($line), PHP_EOL;
  $n = $n + 1;
};
  $sum = 1;
  $i = 1;
  $next = 100;
  while ($i <= 1000) {
  $sum = $sum + totient($i);
  if ($i == $next) {
  echo rtrim('|F(' . _str($i) . ')|: ' . _str($sum)), PHP_EOL;
  $next = $next + 100;
}
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
