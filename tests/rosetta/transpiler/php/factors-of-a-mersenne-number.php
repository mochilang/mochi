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
  $qlimit = 50000;
  function powf($base, $exp) {
  global $qlimit;
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function sqrtApprox($x) {
  global $qlimit;
  if ($x <= 0.0) {
  return 0.0;
}
  $g = $x;
  $i = 0;
  while ($i < 20) {
  $g = ($g + $x / $g) / 2.0;
  $i = $i + 1;
};
  return $g;
};
  function modPow($base, $exp, $mod) {
  global $qlimit;
  $result = 1 % $mod;
  $b = $base % $mod;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $mod;
}
  $b = ($b * $b) % $mod;
  $e = _intdiv($e, 2);
};
  return $result;
};
  function mtest($m) {
  global $qlimit;
  if ($m < 4) {
  echo rtrim(_str($m) . ' < 4.  M' . _str($m) . ' not tested.'), PHP_EOL;
  return;
}
  $flimit = sqrtApprox(powf(2.0, $m) - 1.0);
  $qlast = 0;
  if ($flimit < $qlimit) {
  $qlast = intval($flimit);
} else {
  $qlast = $qlimit;
}
  $composite = [];
  $i = 0;
  while ($i <= $qlast) {
  $composite = array_merge($composite, [false]);
  $i = $i + 1;
};
  $sq = intval(sqrtApprox(floatval($qlast)));
  $q = 3;
  while (true) {
  if ($q <= $sq) {
  $j = $q * $q;
  while ($j <= $qlast) {
  $composite[$j] = true;
  $j = $j + $q;
};
}
  $q8 = $q % 8;
  if (($q8 == 1 || $q8 == 7) && modPow(2, $m, $q) == 1) {
  echo rtrim('M' . _str($m) . ' has factor ' . _str($q)), PHP_EOL;
  return;
}
  while (true) {
  $q = $q + 2;
  if ($q > $qlast) {
  echo rtrim('No factors of M' . _str($m) . ' found.'), PHP_EOL;
  return;
}
  if (!$composite[$q]) {
  break;
}
};
};
};
  function main() {
  global $qlimit;
  mtest(31);
  mtest(67);
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
