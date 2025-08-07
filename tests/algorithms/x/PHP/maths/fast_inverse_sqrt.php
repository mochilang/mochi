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
  function pow2_int($n) {
  $result = 1;
  $i = 0;
  while ($i < $n) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
};
  function pow2_float($n) {
  $result = 1.0;
  if ($n >= 0) {
  $i = 0;
  while ($i < $n) {
  $result = $result * 2.0;
  $i = $i + 1;
};
} else {
  $i = 0;
  $m = 0 - $n;
  while ($i < $m) {
  $result = $result / 2.0;
  $i = $i + 1;
};
}
  return $result;
};
  function lshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
};
  function rshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = _intdiv(($result - ($result % 2)), 2);
  $i = $i + 1;
};
  return $result;
};
  function log2_floor($x) {
  $n = $x;
  $e = 0;
  while ($n >= 2.0) {
  $n = $n / 2.0;
  $e = $e + 1;
};
  while ($n < 1.0) {
  $n = $n * 2.0;
  $e = $e - 1;
};
  return $e;
};
  function float_to_bits($x) {
  $num = $x;
  $sign = 0;
  if ($num < 0.0) {
  $sign = 1;
  $num = -$num;
}
  $exp = log2_floor($num);
  $pow = pow2_float($exp);
  $normalized = $num / $pow;
  $frac = $normalized - 1.0;
  $mantissa = intval(($frac * pow2_float(23)));
  $exp_bits = $exp + 127;
  return lshift($sign, 31) + lshift($exp_bits, 23) + $mantissa;
};
  function bits_to_float($bits) {
  $sign_bit = fmod(rshift($bits, 31), 2);
  $sign = 1.0;
  if ($sign_bit == 1) {
  $sign = -1.0;
}
  $exp_bits = fmod(rshift($bits, 23), 256);
  $exp = $exp_bits - 127;
  $mantissa_bits = fmod($bits, pow2_int(23));
  $mantissa = 1.0 + (floatval($mantissa_bits)) / pow2_float(23);
  return $sign * $mantissa * pow2_float($exp);
};
  function absf($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function is_close($a, $b, $rel_tol) {
  return absf($a - $b) <= $rel_tol * absf($b);
};
  function fast_inverse_sqrt($number) {
  if ($number <= 0.0) {
  $panic('Input must be a positive number.');
}
  $i = float_to_bits($number);
  $magic = 1597463007;
  $y_bits = $magic - rshift($i, 1);
  $y = bits_to_float($y_bits);
  $y = $y * (1.5 - 0.5 * $number * $y * $y);
  return $y;
};
  function test_fast_inverse_sqrt() {
  if (absf(fast_inverse_sqrt(10.0) - 0.3156857923527257) > 0.0001) {
  $panic('fast_inverse_sqrt(10) failed');
}
  if (absf(fast_inverse_sqrt(4.0) - 0.49915357479239103) > 0.0001) {
  $panic('fast_inverse_sqrt(4) failed');
}
  if (absf(fast_inverse_sqrt(4.1) - 0.4932849504615651) > 0.0001) {
  $panic('fast_inverse_sqrt(4.1) failed');
}
  $i = 50;
  while ($i < 60) {
  $y = fast_inverse_sqrt(floatval($i));
  $actual = 1.0 / sqrtApprox(floatval($i));
  if (!is_close($y, $actual, 0.00132)) {
  $panic('relative error too high');
}
  $i = $i + 1;
};
};
  function main() {
  test_fast_inverse_sqrt();
  $i = 5;
  while ($i <= 100) {
  $diff = (1.0 / sqrtApprox(floatval($i))) - fast_inverse_sqrt(floatval($i));
  echo rtrim(_str($i) . ': ' . _str($diff)), PHP_EOL;
  $i = $i + 5;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
