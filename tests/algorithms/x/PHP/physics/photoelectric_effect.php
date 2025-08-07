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
  function pow10($exp) {
  global $PLANCK_CONSTANT_JS, $PLANCK_CONSTANT_EVS;
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
};
  $PLANCK_CONSTANT_JS = 6.6261 / pow10(34);
  $PLANCK_CONSTANT_EVS = 4.1357 / pow10(15);
  function maximum_kinetic_energy($frequency, $work_function, $in_ev) {
  global $PLANCK_CONSTANT_JS, $PLANCK_CONSTANT_EVS;
  if ($frequency < 0.0) {
  _panic('Frequency can\'t be negative.');
}
  $energy = ($in_ev ? $PLANCK_CONSTANT_EVS * $frequency - $work_function : $PLANCK_CONSTANT_JS * $frequency - $work_function);
  if ($energy > 0.0) {
  return $energy;
}
  return 0.0;
};
  echo rtrim(_str(maximum_kinetic_energy(1000000.0, 2.0, false))), PHP_EOL;
  echo rtrim(_str(maximum_kinetic_energy(1000000.0, 2.0, true))), PHP_EOL;
  echo rtrim(_str(maximum_kinetic_energy(10000000000000000.0, 2.0, true))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
