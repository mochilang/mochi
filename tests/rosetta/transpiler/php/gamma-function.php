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
  function ln($x) {
  global $xs;
  $k = 0.0;
  $v = $x;
  while ($v >= 2.0) {
  $v = $v / 2.0;
  $k = $k + 1.0;
};
  while ($v < 1.0) {
  $v = $v * 2.0;
  $k = $k - 1.0;
};
  $z = ($v - 1.0) / ($v + 1.0);
  $zpow = $z;
  $sum = $z;
  $i = 3;
  while ($i <= 9) {
  $zpow = $zpow * $z * $z;
  $sum = $sum + $zpow / (floatval($i));
  $i = $i + 2;
};
  $ln2 = 0.6931471805599453;
  return ($k * $ln2) + 2.0 * $sum;
};
  function expf($x) {
  global $xs;
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 20) {
  $term = $term * $x / floatval($i);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function powf($base, $exp) {
  global $xs;
  return expf($exp * ln($base));
};
  function lanczos7($z) {
  global $xs;
  $t = $z + 6.5;
  $x = 0.9999999999998099 + 676.5203681218851 / $z - 1259.1392167224028 / ($z + 1.0) + 771.3234287776531 / ($z + 2.0) - 176.6150291621406 / ($z + 3.0) + 12.507343278686905 / ($z + 4.0) - 0.13857109526572012 / ($z + 5.0) + 0.000009984369578019572 / ($z + 6.0) + 0.00000015056327351493116 / ($z + 7.0);
  return 2.5066282746310002 * powf($t, $z - 0.5) * powf(2.718281828459045, -$t) * $x;
};
  $xs = [-0.5, 0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 10.0, 140.0, 170.0];
  foreach ($xs as $x) {
  echo rtrim(_str($x) . ' ' . _str(lanczos7($x))), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
