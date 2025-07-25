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
  function d2d($d) {
  return fmod($d, 360.0);
};
  function g2g($g) {
  return fmod($g, 400.0);
};
  function m2m($m) {
  return fmod($m, 6400.0);
};
  function r2r($r) {
  return fmod($r, (2.0 * 3.141592653589793));
};
  function d2g($d) {
  return d2d($d) * 400.0 / 360.0;
};
  function d2m($d) {
  return d2d($d) * 6400.0 / 360.0;
};
  function d2r($d) {
  return d2d($d) * 3.141592653589793 / 180.0;
};
  function g2d($g) {
  return g2g($g) * 360.0 / 400.0;
};
  function g2m($g) {
  return g2g($g) * 6400.0 / 400.0;
};
  function g2r($g) {
  return g2g($g) * 3.141592653589793 / 200.0;
};
  function m2d($m) {
  return m2m($m) * 360.0 / 6400.0;
};
  function m2g($m) {
  return m2m($m) * 400.0 / 6400.0;
};
  function m2r($m) {
  return m2m($m) * 3.141592653589793 / 3200.0;
};
  function r2d($r) {
  return r2r($r) * 180.0 / 3.141592653589793;
};
  function r2g($r) {
  return r2r($r) * 200.0 / 3.141592653589793;
};
  function r2m($r) {
  return r2r($r) * 3200.0 / 3.141592653589793;
};
  function main() {
  $angles = [-2.0, -1.0, 0.0, 1.0, 2.0, 6.2831853, 16.0, 57.2957795, 359.0, 399.0, 6399.0, 1000000.0];
  echo rtrim('degrees normalized_degs gradians mils radians'), PHP_EOL;
  foreach ($angles as $a) {
  echo rtrim(_str($a) . ' ' . _str(d2d($a)) . ' ' . _str(d2g($a)) . ' ' . _str(d2m($a)) . ' ' . _str(d2r($a))), PHP_EOL;
};
  echo rtrim('\ngradians normalized_grds degrees mils radians'), PHP_EOL;
  foreach ($angles as $a) {
  echo rtrim(_str($a) . ' ' . _str(g2g($a)) . ' ' . _str(g2d($a)) . ' ' . _str(g2m($a)) . ' ' . _str(g2r($a))), PHP_EOL;
};
  echo rtrim('\nmils normalized_mils degrees gradians radians'), PHP_EOL;
  foreach ($angles as $a) {
  echo rtrim(_str($a) . ' ' . _str(m2m($a)) . ' ' . _str(m2d($a)) . ' ' . _str(m2g($a)) . ' ' . _str(m2r($a))), PHP_EOL;
};
  echo rtrim('\nradians normalized_rads degrees gradians mils'), PHP_EOL;
  foreach ($angles as $a) {
  echo rtrim(_str($a) . ' ' . _str(r2r($a)) . ' ' . _str(r2d($a)) . ' ' . _str(r2g($a)) . ' ' . _str(r2m($a))), PHP_EOL;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
