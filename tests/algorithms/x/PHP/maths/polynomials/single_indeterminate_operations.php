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
$__start_mem = memory_get_usage();
$__start = _now();
  function copy_list($xs) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function polynomial_new($degree, $coeffs) {
  if (count($coeffs) != $degree + 1) {
  $panic('The number of coefficients should be equal to the degree + 1.');
}
  return ['degree' => $degree, 'coefficients' => copy_list($coeffs)];
};
  function add($p, $q) {
  if ($p['degree'] > $q['degree']) {
  $coeffs = copy_list($p['coefficients']);
  $i = 0;
  while ($i <= $q['degree']) {
  $coeffs[$i] = $coeffs[$i] + $q['coefficients'][$i];
  $i = $i + 1;
};
  return ['degree' => $p['degree'], 'coefficients' => $coeffs];
} else {
  $coeffs = copy_list($q['coefficients']);
  $i = 0;
  while ($i <= $p['degree']) {
  $coeffs[$i] = $coeffs[$i] + $p['coefficients'][$i];
  $i = $i + 1;
};
  return ['degree' => $q['degree'], 'coefficients' => $coeffs];
}
};
  function neg($p) {
  $coeffs = [];
  $i = 0;
  while ($i <= $p['degree']) {
  $coeffs = _append($coeffs, -$p['coefficients'][$i]);
  $i = $i + 1;
};
  return ['degree' => $p['degree'], 'coefficients' => $coeffs];
};
  function sub($p, $q) {
  return add($p, neg($q));
};
  function mul($p, $q) {
  $size = $p['degree'] + $q['degree'] + 1;
  $coeffs = [];
  $i = 0;
  while ($i < $size) {
  $coeffs = _append($coeffs, 0.0);
  $i = $i + 1;
};
  $i = 0;
  while ($i <= $p['degree']) {
  $j = 0;
  while ($j <= $q['degree']) {
  $coeffs[$i + $j] = $coeffs[$i + $j] + $p['coefficients'][$i] * $q['coefficients'][$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return ['degree' => $p['degree'] + $q['degree'], 'coefficients' => $coeffs];
};
  function power($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function evaluate($p, $x) {
  $result = 0.0;
  $i = 0;
  while ($i <= $p['degree']) {
  $result = $result + $p['coefficients'][$i] * power($x, $i);
  $i = $i + 1;
};
  return $result;
};
  function poly_to_string($p) {
  $s = '';
  $i = $p['degree'];
  while ($i >= 0) {
  $coeff = $p['coefficients'][$i];
  if ($coeff != 0.0) {
  if (strlen($s) > 0) {
  if ($coeff > 0.0) {
  $s = $s . ' + ';
} else {
  $s = $s . ' - ';
};
} else {
  if ($coeff < 0.0) {
  $s = $s . '-';
};
};
  $abs_coeff = ($coeff < 0.0 ? -$coeff : $coeff);
  if ($i == 0) {
  $s = $s . _str($abs_coeff);
} else {
  if ($i == 1) {
  $s = $s . _str($abs_coeff) . 'x';
} else {
  $s = $s . _str($abs_coeff) . 'x^' . _str($i);
};
};
}
  $i = $i - 1;
};
  if ($s == '') {
  $s = '0';
}
  return $s;
};
  function derivative($p) {
  if ($p['degree'] == 0) {
  return ['degree' => 0, 'coefficients' => [0.0]];
}
  $coeffs = [];
  $i = 0;
  while ($i < $p['degree']) {
  $coeffs = _append($coeffs, $p['coefficients'][$i + 1] * floatval($i + 1));
  $i = $i + 1;
};
  return ['degree' => $p['degree'] - 1, 'coefficients' => $coeffs];
};
  function integral($p, $constant) {
  $coeffs = [$constant];
  $i = 0;
  while ($i <= $p['degree']) {
  $coeffs = _append($coeffs, $p['coefficients'][$i] / floatval($i + 1));
  $i = $i + 1;
};
  return ['degree' => $p['degree'] + 1, 'coefficients' => $coeffs];
};
  function equals($p, $q) {
  if ($p['degree'] != $q['degree']) {
  return false;
}
  $i = 0;
  while ($i <= $p['degree']) {
  if ($p['coefficients'][$i] != $q['coefficients'][$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function not_equals($p, $q) {
  return !equals($p, $q);
};
  function test_polynomial() {
  $p = polynomial_new(2, [1.0, 2.0, 3.0]);
  $q = polynomial_new(2, [1.0, 2.0, 3.0]);
  if (poly_to_string(add($p, $q)) != '6x^2 + 4x + 2') {
  $panic('add failed');
}
  if (poly_to_string(sub($p, $q)) != '0') {
  $panic('sub failed');
}
  if (evaluate($p, 2.0) != 17.0) {
  $panic('evaluate failed');
}
  if (poly_to_string(derivative($p)) != '6x + 2') {
  $panic('derivative failed');
}
  $integ = poly_to_string(integral($p, 0.0));
  if ($integ != '1x^3 + 1x^2 + 1x') {
  $panic('integral failed');
}
  if (!equals($p, $q)) {
  $panic('equals failed');
}
  if (not_equals($p, $q)) {
  $panic('not_equals failed');
}
};
  function main() {
  test_polynomial();
  $p = polynomial_new(2, [1.0, 2.0, 3.0]);
  $d = derivative($p);
  echo rtrim(poly_to_string($d)), PHP_EOL;
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
