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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function complex_conj($z) {
  global $a, $v, $r1, $b, $r2;
  return ['re' => $z['re'], 'im' => -$z['im']];
};
  function complex_eq($a, $b) {
  global $v, $r1, $r2;
  return $a['re'] == $b['re'] && $a['im'] == $b['im'];
};
  function complex_add($a, $b) {
  global $v, $r1, $r2;
  return ['re' => $a['re'] + $b['re'], 'im' => $a['im'] + $b['im']];
};
  function complex_mul($a, $b) {
  global $v, $r1, $r2;
  $real = $a['re'] * $b['re'] - $a['im'] * $b['im'];
  $imag = $a['re'] * $b['im'] + $a['im'] * $b['re'];
  return ['re' => $real, 'im' => $imag];
};
  function conj_vector($v) {
  global $a, $r1, $b, $r2;
  $res = [];
  $i = 0;
  while ($i < count($v)) {
  $res = _append($res, complex_conj($v[$i]));
  $i = $i + 1;
};
  return $res;
};
  function vec_mat_mul($v, $m) {
  global $a, $r1, $b, $r2;
  $result = [];
  $col = 0;
  while ($col < count($m[0])) {
  $sum = ['re' => 0.0, 'im' => 0.0];
  $row = 0;
  while ($row < count($v)) {
  $sum = complex_add($sum, complex_mul($v[$row], $m[$row][$col]));
  $row = $row + 1;
};
  $result = _append($result, $sum);
  $col = $col + 1;
};
  return $result;
};
  function dot($a, $b) {
  global $v, $r1, $r2;
  $sum = ['re' => 0.0, 'im' => 0.0];
  $i = 0;
  while ($i < count($a)) {
  $sum = complex_add($sum, complex_mul($a[$i], $b[$i]));
  $i = $i + 1;
};
  return $sum;
};
  function is_hermitian($m) {
  global $a, $v, $r1, $b, $r2;
  $i = 0;
  while ($i < count($m)) {
  $j = 0;
  while ($j < count($m)) {
  if (!complex_eq($m[$i][$j], complex_conj($m[$j][$i]))) {
  return false;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return true;
};
  function rayleigh_quotient($a, $v) {
  global $r1, $b, $r2;
  $v_star = conj_vector($v);
  $v_star_dot = vec_mat_mul($v_star, $a);
  $num = dot($v_star_dot, $v);
  $den = dot($v_star, $v);
  return $num['re'] / $den['re'];
};
  $a = [[['re' => 2.0, 'im' => 0.0], ['re' => 2.0, 'im' => 1.0], ['re' => 4.0, 'im' => 0.0]], [['re' => 2.0, 'im' => -1.0], ['re' => 3.0, 'im' => 0.0], ['re' => 0.0, 'im' => 1.0]], [['re' => 4.0, 'im' => 0.0], ['re' => 0.0, 'im' => -1.0], ['re' => 1.0, 'im' => 0.0]]];
  $v = [['re' => 1.0, 'im' => 0.0], ['re' => 2.0, 'im' => 0.0], ['re' => 3.0, 'im' => 0.0]];
  if (is_hermitian($a)) {
  $r1 = rayleigh_quotient($a, $v);
  echo rtrim(json_encode($r1, 1344)), PHP_EOL;
  echo rtrim('
'), PHP_EOL;
}
  $b = [[['re' => 1.0, 'im' => 0.0], ['re' => 2.0, 'im' => 0.0], ['re' => 4.0, 'im' => 0.0]], [['re' => 2.0, 'im' => 0.0], ['re' => 3.0, 'im' => 0.0], ['re' => -1.0, 'im' => 0.0]], [['re' => 4.0, 'im' => 0.0], ['re' => -1.0, 'im' => 0.0], ['re' => 1.0, 'im' => 0.0]]];
  if (is_hermitian($b)) {
  $r2 = rayleigh_quotient($b, $v);
  echo rtrim(json_encode($r2, 1344)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
