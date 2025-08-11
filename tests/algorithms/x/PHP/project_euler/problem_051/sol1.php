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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  function parse_int($s) {
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $value = _iadd(_imul($value, 10), ((ctype_digit($s[$i]) ? intval($s[$i]) : ord($s[$i]))));
  $i = _iadd($i, 1);
};
  return $value;
};
  function digit_replacements($number) {
  $num_str = _str($number);
  $counts = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
  $i = 0;
  while ($i < strlen($num_str)) {
  $d = ((ctype_digit($num_str[$i]) ? intval($num_str[$i]) : ord($num_str[$i])));
  $counts[$d] = _iadd($counts[$d], 1);
  $i = _iadd($i, 1);
};
  $result = [];
  $digits = '0123456789';
  $digit = 0;
  while ($digit < 10) {
  if ($counts[$digit] > 1) {
  $family = [];
  $repl = 0;
  while ($repl < 10) {
  $new_str = '';
  $j = 0;
  while ($j < strlen($num_str)) {
  $c = substr($num_str, $j, $j + 1 - $j);
  if ($c == substr($digits, $digit, $digit + 1 - $digit)) {
  $new_str = $new_str . substr($digits, $repl, $repl + 1 - $repl);
} else {
  $new_str = $new_str . $c;
}
  $j = _iadd($j, 1);
};
  $family = _append($family, parse_int($new_str));
  $repl = _iadd($repl, 1);
};
  $result = _append($result, $family);
}
  $digit = _iadd($digit, 1);
};
  return $result;
};
  function is_prime($num) {
  if ($num < 2) {
  return false;
}
  if (_imod($num, 2) == 0) {
  return $num == 2;
}
  $i = 3;
  while (_imul($i, $i) <= $num) {
  if (_imod($num, $i) == 0) {
  return false;
}
  $i = _iadd($i, 2);
};
  return true;
};
  function solution($family_length) {
  $candidate = 121313;
  if (!is_prime($candidate)) {
  return -1;
}
  $reps = digit_replacements($candidate);
  $r = 0;
  while ($r < count($reps)) {
  $family = $reps[$r];
  $count = 0;
  $min_prime = 0;
  $first = true;
  $i = 0;
  while ($i < count($family)) {
  $num = $family[$i];
  if (is_prime($num)) {
  if ($first) {
  $min_prime = $num;
  $first = false;
} else {
  if ($num < $min_prime) {
  $min_prime = $num;
};
};
  $count = _iadd($count, 1);
}
  $i = _iadd($i, 1);
};
  if ($count == $family_length) {
  return $min_prime;
}
  $r = _iadd($r, 1);
};
  return -1;
};
  echo rtrim(_str(solution(8))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
