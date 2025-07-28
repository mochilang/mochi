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
  $bCoeff = 7.0;
  function zero() {
  global $bCoeff;
  return ['x' => 0.0, 'y' => 0.0, 'inf' => true];
};
  function isZero($p) {
  global $bCoeff;
  return $p['inf'];
};
  function neg($p) {
  global $bCoeff;
  return ['x' => $p['x'], 'y' => -$p['y'], 'inf' => $p['inf']];
};
  function dbl($p) {
  global $bCoeff;
  if (isZero($p)) {
  return $p;
}
  $L = (3.0 * $p['x'] * $p['x']) / (2.0 * $p['y']);
  $x = $L * $L - 2.0 * $p['x'];
  return ['x' => $x, 'y' => $L * ($p['x'] - $x) - $p['y'], 'inf' => false];
};
  function add($p, $q) {
  global $bCoeff;
  if (isZero($p)) {
  return $q;
}
  if (isZero($q)) {
  return $p;
}
  if ($p['x'] == $q['x']) {
  if ($p['y'] == $q['y']) {
  return dbl($p);
};
  return zero();
}
  $L = ($q['y'] - $p['y']) / ($q['x'] - $p['x']);
  $x = $L * $L - $p['x'] - $q['x'];
  return ['x' => $x, 'y' => $L * ($p['x'] - $x) - $p['y'], 'inf' => false];
};
  function mul($p, $n) {
  global $bCoeff;
  $r = zero();
  $q = $p;
  $k = $n;
  while ($k > 0) {
  if ($k % 2 == 1) {
  $r = add($r, $q);
}
  $q = dbl($q);
  $k = _intdiv($k, 2);
};
  return $r;
};
  function cbrtApprox($x) {
  global $bCoeff;
  $guess = $x;
  $i = 0;
  while ($i < 40) {
  $guess = (2.0 * $guess + $x / ($guess * $guess)) / 3.0;
  $i = $i + 1;
};
  return $guess;
};
  function fromY($y) {
  global $bCoeff;
  return ['x' => cbrtApprox($y * $y - $bCoeff), 'y' => $y, 'inf' => false];
};
  function show($s, $p) {
  global $bCoeff;
  if (isZero($p)) {
  echo rtrim($s . 'Zero'), PHP_EOL;
} else {
  echo rtrim($s . '(' . _str($p['x']) . ', ' . _str($p['y']) . ')'), PHP_EOL;
}
};
  function main() {
  global $bCoeff;
  $a = fromY(1.0);
  $b = fromY(2.0);
  show('a = ', $a);
  show('b = ', $b);
  $c = add($a, $b);
  show('c = a + b = ', $c);
  $d = neg($c);
  show('d = -c = ', $d);
  show('c + d = ', add($c, $d));
  show('a + b + d = ', add($a, add($b, $d)));
  show('a * 12345 = ', mul($a, 12345));
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
