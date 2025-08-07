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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $c = 299792458.0;
  function sqrtApprox($x) {
  global $c, $v;
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
  function beta($velocity) {
  global $c, $v;
  if ($velocity > $c) {
  _panic('Speed must not exceed light speed 299,792,458 [m/s]!');
}
  if ($velocity < 1.0) {
  _panic('Speed must be greater than or equal to 1!');
}
  return $velocity / $c;
};
  function gamma($velocity) {
  global $c, $v;
  $b = beta($velocity);
  return 1.0 / sqrtApprox(1.0 - $b * $b);
};
  function transformation_matrix($velocity) {
  global $c, $v;
  $g = gamma($velocity);
  $b = beta($velocity);
  return [[$g, -$g * $b, 0.0, 0.0], [-$g * $b, $g, 0.0, 0.0], [0.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 1.0]];
};
  function mat_vec_mul($mat, $vec) {
  global $c, $v;
  $res = [];
  $i = 0;
  while ($i < 4) {
  $row = $mat[$i];
  $value = $row[0] * $vec[0] + $row[1] * $vec[1] + $row[2] * $vec[2] + $row[3] * $vec[3];
  $res = array_merge($res, [$value]);
  $i = $i + 1;
};
  return $res;
};
  function transform($velocity, $event) {
  global $c, $v;
  $g = gamma($velocity);
  $b = beta($velocity);
  $ct = $event[0] * $c;
  $x = $event[1];
  return [$g * $ct - $g * $b * $x, -$g * $b * $ct + $g * $x, $event[2], $event[3]];
};
  echo rtrim(_str(beta($c))), PHP_EOL;
  echo rtrim(_str(beta(199792458.0))), PHP_EOL;
  echo rtrim(_str(beta(100000.0))), PHP_EOL;
  echo rtrim(_str(gamma(4.0))), PHP_EOL;
  echo rtrim(_str(gamma(100000.0))), PHP_EOL;
  echo rtrim(_str(gamma(30000000.0))), PHP_EOL;
  echo rtrim(_str(transformation_matrix(29979245.0))), PHP_EOL;
  $v = transform(29979245.0, [1.0, 2.0, 3.0, 4.0]);
  echo rtrim(_str($v)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
