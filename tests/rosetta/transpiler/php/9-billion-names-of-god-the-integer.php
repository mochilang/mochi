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
  function bigTrim($a) {
  global $x, $r, $line, $i;
  $n = count($a);
  while ($n > 1 && $a[$n - 1] == 0) {
  $a = array_slice($a, 0, $n - 1 - 0);
  $n = $n - 1;
};
  return $a;
};
  function bigFromInt($x) {
  global $r, $line, $i;
  if ($x == 0) {
  return [0];
}
  $digits = [];
  $n = $x;
  while ($n > 0) {
  $digits = array_merge($digits, [$n % 10]);
  $n = _intdiv($n, 10);
};
  return $digits;
};
  function bigAdd($a, $b) {
  global $x, $r, $line;
  $res = [];
  $carry = 0;
  $i = 0;
  while ($i < count($a) || $i < count($b) || $carry > 0) {
  $av = 0;
  if ($i < count($a)) {
  $av = $a[$i];
}
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $s = $av + $bv + $carry;
  $res = array_merge($res, [$s % 10]);
  $carry = _intdiv($s, 10);
  $i = $i + 1;
};
  return bigTrim($res);
};
  function bigSub($a, $b) {
  global $x, $r, $line;
  $res = [];
  $borrow = 0;
  $i = 0;
  while ($i < count($a)) {
  $av = $a[$i];
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $diff = $av - $bv - $borrow;
  if ($diff < 0) {
  $diff = $diff + 10;
  $borrow = 1;
} else {
  $borrow = 0;
}
  $res = array_merge($res, [$diff]);
  $i = $i + 1;
};
  return bigTrim($res);
};
  function bigToString($a) {
  global $x, $r, $line;
  $s = '';
  $i = count($a) - 1;
  while ($i >= 0) {
  $s = $s . _str($a[$i]);
  $i = $i - 1;
};
  return $s;
};
  function minInt($a, $b) {
  global $x, $r, $line, $i;
  if ($a < $b) {
  return $a;
} else {
  return $b;
}
};
  function cumu($n) {
  global $r, $line, $i;
  $cache = [[bigFromInt(1)]];
  $y = 1;
  while ($y <= $n) {
  $row = [bigFromInt(0)];
  $x = 1;
  while ($x <= $y) {
  $val = $cache[$y - $x][minInt($x, $y - $x)];
  $row = array_merge($row, [bigAdd($row[count($row) - 1], $val)]);
  $x = $x + 1;
};
  $cache = array_merge($cache, [$row]);
  $y = $y + 1;
};
  return $cache[$n];
};
  function row($n) {
  global $x, $r, $line;
  $e = cumu($n);
  $out = [];
  $i = 0;
  while ($i < $n) {
  $diff = bigSub($e[$i + 1], $e[$i]);
  $out = array_merge($out, [bigToString($diff)]);
  $i = $i + 1;
};
  return $out;
};
  echo rtrim('rows:'), PHP_EOL;
  $x = 1;
  while ($x < 11) {
  $r = row($x);
  $line = '';
  $i = 0;
  while ($i < count($r)) {
  $line = $line . ' ' . $r[$i] . ' ';
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
  $x = $x + 1;
}
  echo rtrim(''), PHP_EOL;
  echo rtrim('sums:'), PHP_EOL;
  foreach ([23, 123, 1234] as $num) {
  $r = cumu($num);
  echo rtrim(_str($num) . ' ' . bigToString($r[count($r) - 1])), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
