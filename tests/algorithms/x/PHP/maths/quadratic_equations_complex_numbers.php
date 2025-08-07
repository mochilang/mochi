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
  function add($a, $b) {
  return ['re' => $a['re'] + $b['re'], 'im' => $a['im'] + $b['im']];
};
  function sub($a, $b) {
  return ['re' => $a['re'] - $b['re'], 'im' => $a['im'] - $b['im']];
};
  function div_real($a, $r) {
  return ['re' => $a['re'] / $r, 'im' => $a['im'] / $r];
};
  function sqrt_newton($x) {
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
  function sqrt_to_complex($d) {
  if ($d >= 0.0) {
  return ['re' => sqrt_newton($d), 'im' => 0.0];
}
  return ['re' => 0.0, 'im' => sqrt_newton(-$d)];
};
  function quadratic_roots($a, $b, $c) {
  if ($a == 0.0) {
  echo rtrim('ValueError: coefficient \'a\' must not be zero'), PHP_EOL;
  return [];
}
  $delta = $b * $b - 4.0 * $a * $c;
  $sqrt_d = sqrt_to_complex($delta);
  $minus_b = ['re' => -$b, 'im' => 0.0];
  $two_a = 2.0 * $a;
  $root1 = div_real(add($minus_b, $sqrt_d), $two_a);
  $root2 = div_real(sub($minus_b, $sqrt_d), $two_a);
  return [$root1, $root2];
};
  function root_str($r) {
  if ($r['im'] == 0.0) {
  return _str($r['re']);
}
  $s = _str($r['re']);
  if ($r['im'] >= 0.0) {
  $s = $s . '+' . _str($r['im']) . 'i';
} else {
  $s = $s . _str($r['im']) . 'i';
}
  return $s;
};
  function main() {
  $roots = quadratic_roots(5.0, 6.0, 1.0);
  if (count($roots) == 2) {
  echo rtrim('The solutions are: ' . root_str($roots[0]) . ' and ' . root_str($roots[1])), PHP_EOL;
}
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
