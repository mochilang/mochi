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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
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
  $MAX = 4294967296;
  $HALF = 2147483648;
  function to_unsigned($n) {
  global $MAX, $HALF;
  if ($n < 0) {
  return _iadd($MAX, $n);
}
  return $n;
};
  function from_unsigned($n) {
  global $MAX, $HALF;
  if ($n >= $HALF) {
  return _isub($n, $MAX);
}
  return $n;
};
  function bit_and($a, $b) {
  global $MAX, $HALF;
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  $i = 0;
  while ($i < 32) {
  if ((_imod($x, 2) == 1) && (_imod($y, 2) == 1)) {
  $res = _iadd($res, $bit);
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = _imul($bit, 2);
  $i = _iadd($i, 1);
};
  return $res;
};
  function bit_xor($a, $b) {
  global $MAX, $HALF;
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  $i = 0;
  while ($i < 32) {
  $abit = _imod($x, 2);
  $bbit = _imod($y, 2);
  if (_imod((_iadd($abit, $bbit)), 2) == 1) {
  $res = _iadd($res, $bit);
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = _imul($bit, 2);
  $i = _iadd($i, 1);
};
  return $res;
};
  function lshift1($num) {
  global $MAX, $HALF;
  return _imod((_imul($num, 2)), $MAX);
};
  function add($a, $b) {
  global $MAX, $HALF;
  $first = to_unsigned($a);
  $second = to_unsigned($b);
  while ($second != 0) {
  $carry = bit_and($first, $second);
  $first = bit_xor($first, $second);
  $second = lshift1($carry);
};
  $result = from_unsigned($first);
  return $result;
};
  echo rtrim(_str(add(3, 5))), PHP_EOL;
  echo rtrim(_str(add(13, 5))), PHP_EOL;
  echo rtrim(_str(add(-7, 2))), PHP_EOL;
  echo rtrim(_str(add(0, -7))), PHP_EOL;
  echo rtrim(_str(add(-321, 0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
