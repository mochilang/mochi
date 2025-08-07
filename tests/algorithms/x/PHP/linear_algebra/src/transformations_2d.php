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
  $PI = 3.141592653589793;
  function mochi_floor($x) {
  global $PI;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function modf($x, $m) {
  global $PI;
  return $x - mochi_floor($x / $m) * $m;
};
  function sin_taylor($angle) {
  global $PI;
  $x = modf($angle, 2.0 * $PI);
  if ($x > $PI) {
  $x = $x - 2.0 * $PI;
}
  $term = $x;
  $sum = $x;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i));
  $k2 = $k1 + 1.0;
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function cos_taylor($angle) {
  global $PI;
  $x = modf($angle, 2.0 * $PI);
  if ($x > $PI) {
  $x = $x - 2.0 * $PI;
}
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i)) - 1.0;
  $k2 = 2.0 * (floatval($i));
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function matrix_to_string($m) {
  global $PI;
  $s = '[';
  $i = 0;
  while ($i < count($m)) {
  $row = $m[$i];
  $s = $s . '[';
  $j = 0;
  while ($j < count($row)) {
  $s = $s . _str($row[$j]);
  if ($j < count($row) - 1) {
  $s = $s . ', ';
}
  $j = $j + 1;
};
  $s = $s . ']';
  if ($i < count($m) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function scaling($f) {
  global $PI;
  return [[$f, 0.0], [0.0, $f]];
};
  function rotation($angle) {
  global $PI;
  $c = cos_taylor($angle);
  $s = sin_taylor($angle);
  return [[$c, -$s], [$s, $c]];
};
  function projection($angle) {
  global $PI;
  $c = cos_taylor($angle);
  $s = sin_taylor($angle);
  $cs = $c * $s;
  return [[$c * $c, $cs], [$cs, $s * $s]];
};
  function reflection($angle) {
  global $PI;
  $c = cos_taylor($angle);
  $s = sin_taylor($angle);
  $cs = $c * $s;
  return [[2.0 * $c - 1.0, 2.0 * $cs], [2.0 * $cs, 2.0 * $s - 1.0]];
};
  echo rtrim('    scaling(5) = ' . matrix_to_string(scaling(5.0))), PHP_EOL;
  echo rtrim('  rotation(45) = ' . matrix_to_string(rotation(45.0))), PHP_EOL;
  echo rtrim('projection(45) = ' . matrix_to_string(projection(45.0))), PHP_EOL;
  echo rtrim('reflection(45) = ' . matrix_to_string(reflection(45.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
