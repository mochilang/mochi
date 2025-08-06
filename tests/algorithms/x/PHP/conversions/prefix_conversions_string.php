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
$__start_mem = memory_get_usage();
$__start = _now();
  $si_positive = [['name' => 'yotta', 'exp' => 24], ['name' => 'zetta', 'exp' => 21], ['name' => 'exa', 'exp' => 18], ['name' => 'peta', 'exp' => 15], ['name' => 'tera', 'exp' => 12], ['name' => 'giga', 'exp' => 9], ['name' => 'mega', 'exp' => 6], ['name' => 'kilo', 'exp' => 3], ['name' => 'hecto', 'exp' => 2], ['name' => 'deca', 'exp' => 1]];
  $si_negative = [['name' => 'deci', 'exp' => -1], ['name' => 'centi', 'exp' => -2], ['name' => 'milli', 'exp' => -3], ['name' => 'micro', 'exp' => -6], ['name' => 'nano', 'exp' => -9], ['name' => 'pico', 'exp' => -12], ['name' => 'femto', 'exp' => -15], ['name' => 'atto', 'exp' => -18], ['name' => 'zepto', 'exp' => -21], ['name' => 'yocto', 'exp' => -24]];
  $binary_prefixes = [['name' => 'yotta', 'exp' => 80], ['name' => 'zetta', 'exp' => 70], ['name' => 'exa', 'exp' => 60], ['name' => 'peta', 'exp' => 50], ['name' => 'tera', 'exp' => 40], ['name' => 'giga', 'exp' => 30], ['name' => 'mega', 'exp' => 20], ['name' => 'kilo', 'exp' => 10]];
  function mochi_pow($base, $exp) {
  global $si_positive, $si_negative, $binary_prefixes;
  $result = 1.0;
  $e = $exp;
  if ($e < 0) {
  $e = -$e;
  $i = 0;
  while ($i < $e) {
  $result = $result * $base;
  $i = $i + 1;
};
  return 1.0 / $result;
}
  $i = 0;
  while ($i < $e) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function add_si_prefix($value) {
  global $si_positive, $si_negative, $binary_prefixes;
  $prefixes = null;
  if ($value > 0.0) {
  $prefixes = $si_positive;
} else {
  $prefixes = $si_negative;
}
  $i = 0;
  while ($i < count($prefixes)) {
  $p = $prefixes[$i];
  $num = $value / mochi_pow(10.0, $p['exp']);
  if ($num > 1.0) {
  return _str($num) . ' ' . $p['name'];
}
  $i = $i + 1;
};
  return _str($value);
};
  function add_binary_prefix($value) {
  global $si_positive, $si_negative, $binary_prefixes;
  $i = 0;
  while ($i < count($binary_prefixes)) {
  $p = $binary_prefixes[$i];
  $num = $value / mochi_pow(2.0, $p['exp']);
  if ($num > 1.0) {
  return _str($num) . ' ' . $p['name'];
}
  $i = $i + 1;
};
  return _str($value);
};
  echo rtrim(add_si_prefix(10000.0)), PHP_EOL;
  echo rtrim(add_si_prefix(0.005)), PHP_EOL;
  echo rtrim(add_binary_prefix(65536.0)), PHP_EOL;
  echo rtrim(add_binary_prefix(512.0)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
