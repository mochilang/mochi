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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  function sinApprox($x) {
  global $PI;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 10) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function cosApprox($x) {
  global $PI;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n <= 10) {
  $denom = floatval(((2 * $n - 1) * (2 * $n)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function cis($x) {
  global $PI;
  return ['re' => cosApprox($x), 'im' => sinApprox($x)];
};
  function add($a, $b) {
  global $PI;
  return ['re' => $a['re'] + $b['re'], 'im' => $a['im'] + $b['im']];
};
  function sub($a, $b) {
  global $PI;
  return ['re' => $a['re'] - $b['re'], 'im' => $a['im'] - $b['im']];
};
  function mul($a, $b) {
  global $PI;
  return ['re' => $a['re'] * $b['re'] - $a['im'] * $b['im'], 'im' => $a['re'] * $b['im'] + $a['im'] * $b['re']];
};
  function ditfft2Rec($x, &$y, $offX, $offY, $n, $s) {
  global $PI;
  if ($n == 1) {
  $y[$offY] = ['re' => $x[$offX], 'im' => 0.0];
  return;
}
  ditfft2Rec($x, $y, $offX, $offY, _intdiv($n, 2), 2 * $s);
  ditfft2Rec($x, $y, $offX + $s, $offY + _intdiv($n, 2), _intdiv($n, 2), 2 * $s);
  $k = 0;
  while ($k < _intdiv($n, 2)) {
  $angle = -2.0 * $PI * (floatval($k)) / (floatval($n));
  $tf = mul(cis($angle), $y[$offY + $k + _intdiv($n, 2)]);
  $a = add($y[$offY + $k], $tf);
  $b = sub($y[$offY + $k], $tf);
  $y[$offY + $k] = $a;
  $y[$offY + $k + _intdiv($n, 2)] = $b;
  $k = $k + 1;
};
};
  function ditfft2($x, &$y, $n, $s) {
  global $PI;
  ditfft2Rec($x, $y, 0, 0, $n, $s);
};
  function main() {
  global $PI;
  $x = [1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0];
  $y = [];
  $i = 0;
  while ($i < count($x)) {
  $y = array_merge($y, [['re' => 0.0, 'im' => 0.0]]);
  $i = $i + 1;
};
  ditfft2($x, $y, count($x), 1);
  foreach ($y as $c) {
  $line = pad(fmt($c['re']), 8);
  if ($c['im'] >= 0) {
  $line = $line . '+' . fmt($c['im']);
} else {
  $line = $line . fmt($c['im']);
}
  echo rtrim($line), PHP_EOL;
};
};
  function pad($s, $w) {
  global $PI;
  $t = $s;
  while (strlen($t) < $w) {
  $t = ' ' . $t;
};
  return $t;
};
  function fmt($x) {
  global $PI;
  $y = floorf($x * 10000.0 + 0.5) / 10000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.0000';
} else {
  $d = strlen($s) - $dot - 1;
  while ($d < 4) {
  $s = $s . '0';
  $d = $d + 1;
};
}
  return $s;
};
  function floorf($x) {
  global $PI;
  $y = intval($x);
  return floatval($y);
};
  function indexOf($s, $ch) {
  global $PI;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
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
