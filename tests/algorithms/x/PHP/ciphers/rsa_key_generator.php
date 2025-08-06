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
  function pow2($exp) {
  global $seed, $keys, $pub, $priv;
  $res = 1;
  $i = 0;
  while ($i < $exp) {
  $res = $res * 2;
  $i = $i + 1;
};
  return $res;
};
  $seed = 1;
  function next_seed($x) {
  global $seed, $keys, $pub, $priv;
  return ($x * 1103515245 + 12345) % 2147483648;
};
  function rand_range($min, $max) {
  global $seed, $keys, $pub, $priv;
  $seed = next_seed($seed);
  return $min + $seed % ($max - $min);
};
  function gcd($a, $b) {
  global $seed, $keys, $pub, $priv;
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $temp = $x % $y;
  $x = $y;
  $y = $temp;
};
  return $x;
};
  function mod_inverse($e, $phi) {
  global $seed, $keys, $pub, $priv;
  $t = 0;
  $newt = 1;
  $r = $phi;
  $newr = $e;
  while ($newr != 0) {
  $quotient = _intdiv($r, $newr);
  $tmp = $newt;
  $newt = $t - $quotient * $newt;
  $t = $tmp;
  $tmp_r = $newr;
  $newr = $r - $quotient * $newr;
  $r = $tmp_r;
};
  if ($r > 1) {
  return 0;
}
  if ($t < 0) {
  $t = $t + $phi;
}
  return $t;
};
  function is_prime($n) {
  global $seed, $keys, $pub, $priv;
  if ($n < 2) {
  return false;
}
  $i = 2;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function generate_prime($bits) {
  global $seed, $keys, $pub, $priv;
  $min = pow2($bits - 1);
  $max = pow2($bits);
  $p = rand_range($min, $max);
  if ($p % 2 == 0) {
  $p = $p + 1;
}
  while (!is_prime($p)) {
  $p = $p + 2;
  if ($p >= $max) {
  $p = $min + 1;
}
};
  return $p;
};
  function generate_key($bits) {
  global $seed, $keys, $pub, $priv;
  $p = generate_prime($bits);
  $q = generate_prime($bits);
  $n = $p * $q;
  $phi = ($p - 1) * ($q - 1);
  $e = rand_range(2, $phi);
  while (gcd($e, $phi) != 1) {
  $e = $e + 1;
  if ($e >= $phi) {
  $e = 2;
}
};
  $d = mod_inverse($e, $phi);
  return ['public_key' => [$n, $e], 'private_key' => [$n, $d]];
};
  $keys = generate_key(8);
  $pub = $keys['public_key'];
  $priv = $keys['private_key'];
  echo rtrim('Public key: (' . _str($pub[0]) . ', ' . _str($pub[1]) . ')'), PHP_EOL;
  echo rtrim('Private key: (' . _str($priv[0]) . ', ' . _str($priv[1]) . ')'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
