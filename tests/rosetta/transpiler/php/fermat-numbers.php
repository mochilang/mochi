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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow_int($base, $exp) {
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
  function pow_big($base, $exp) {
  $result = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = _imul($result, $b);
}
  $b = _imul($b, $b);
  $e = intval((_intdiv($e, 2)));
};
  return $result;
};
  function parseBigInt($str) {
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  while ($i < strlen($str)) {
  $ch = substr($str, $i, $i + 1 - $i);
  $d = intval($ch);
  $n = _imul($n, (10)) + ($d);
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function fermat($n) {
  $p = pow_int(2, $n);
  return _iadd(pow_big(2, $p), (1));
};
  function primeFactorsBig($n) {
  $factors = [];
  $m = $n;
  $d = 2;
  while (_imod($m, $d) == 0) {
  $factors = array_merge($factors, [$d]);
  $m = _idiv($m, $d);
};
  $d = 3;
  while (_imul($d, $d) <= $m) {
  while (_imod($m, $d) == 0) {
  $factors = array_merge($factors, [$d]);
  $m = _idiv($m, $d);
};
  $d = _iadd($d, 2);
};
  if ($m > 1) {
  $factors = array_merge($factors, [$m]);
}
  return $factors;
};
  function show_list($xs) {
  $line = '';
  $i = 0;
  while ($i < count($xs)) {
  $line = $line . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $line = $line . ' ';
}
  $i = $i + 1;
};
  return $line;
};
  function main() {
  $nums = [];
  for ($i = 0; $i < 8; $i++) {
  $nums = array_merge($nums, [fermat($i)]);
};
  echo rtrim('First 8 Fermat numbers:'), PHP_EOL;
  foreach ($nums as $n) {
  echo rtrim(_str($n)), PHP_EOL;
};
  $extra = [6 => [274177, 67280421310721], 7 => [parseBigInt('59649589127497217'), parseBigInt('5704689200685129054721')]];
  echo rtrim('
Factors:'), PHP_EOL;
  $i = 0;
  while ($i < count($nums)) {
  $facs = [];
  if ($i <= 5) {
  $facs = primeFactorsBig($nums[$i]);
} else {
  $facs = $extra[$i];
}
  echo rtrim('F' . _str($i) . ' = ' . show_list($facs)), PHP_EOL;
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
