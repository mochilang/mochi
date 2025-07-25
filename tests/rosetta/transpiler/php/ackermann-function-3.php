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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow_big($base, $exp) {
  global $err;
  $result = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = $result * $b;
}
  $b = $b * $b;
  $e = intval((_intdiv($e, 2)));
};
  return $result;
};
  function bit_len($x) {
  global $err;
  $n = $x;
  $c = 0;
  while ($n > 0) {
  $n = $n / 2;
  $c = $c + 1;
};
  return $c;
};
  $err = '';
  function ackermann2($m, $n) {
  global $err;
  if ($err != '') {
  return 0;
}
  if ($m <= 3) {
  $mi = intval($m);
  if ($mi == 0) {
  return $n + 1;
};
  if ($mi == 1) {
  return $n + 2;
};
  if ($mi == 2) {
  return 2 * $n + 3;
};
  if ($mi == 3) {
  $nb = bit_len($n);
  if ($nb > 64) {
  $err = 'A(m,n) had n of ' . _str($nb) . ' bits; too large';
  return 0;
};
  $r = pow_big(2, intval($n));
  return 8 * $r - 3;
};
}
  if (bit_len($n) == 0) {
  return ackermann2($m - (1), 1);
}
  return ackermann2($m - (1), ackermann2($m, $n - (1)));
};
  function show($m, $n) {
  global $err;
  $err = '';
  $res = ackermann2($m, $n);
  if ($err != '') {
  echo rtrim('A(' . _str($m) . ', ' . _str($n) . ') = Error: ' . $err), PHP_EOL;
  return;
}
  if (bit_len($res) <= 256) {
  echo rtrim('A(' . _str($m) . ', ' . _str($n) . ') = ' . _str($res)), PHP_EOL;
} else {
  $s = _str($res);
  $pre = substr($s, 0, 20 - 0);
  $suf = substr($s, strlen($s) - 20, strlen($s) - (strlen($s) - 20));
  echo rtrim('A(' . _str($m) . ', ' . _str($n) . ') = ' . _str(strlen($s)) . ' digits starting/ending with: ' . $pre . '...' . $suf), PHP_EOL;
}
};
  function main() {
  global $err;
  show(0, 0);
  show(1, 2);
  show(2, 4);
  show(3, 100);
  show(3, 1000000);
  show(4, 1);
  show(4, 2);
  show(4, 3);
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
