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
  function round2($x) {
  global $r1, $r2;
  $scaled = $x * 100.0;
  $rounded = floatval((intval(($scaled + 0.5))));
  return $rounded / 100.0;
};
  function center_of_mass($ps) {
  global $r1, $r2;
  if (count($ps) == 0) {
  _panic('No particles provided');
}
  $i = 0;
  $total_mass = 0.0;
  while ($i < count($ps)) {
  $p = $ps[$i];
  if ($p['mass'] <= 0.0) {
  _panic('Mass of all particles must be greater than 0');
}
  $total_mass = $total_mass + $p['mass'];
  $i = $i + 1;
};
  $sum_x = 0.0;
  $sum_y = 0.0;
  $sum_z = 0.0;
  $i = 0;
  while ($i < count($ps)) {
  $p = $ps[$i];
  $sum_x = $sum_x + $p['x'] * $p['mass'];
  $sum_y = $sum_y + $p['y'] * $p['mass'];
  $sum_z = $sum_z + $p['z'] * $p['mass'];
  $i = $i + 1;
};
  $cm_x = round2($sum_x / $total_mass);
  $cm_y = round2($sum_y / $total_mass);
  $cm_z = round2($sum_z / $total_mass);
  return ['x' => $cm_x, 'y' => $cm_y, 'z' => $cm_z];
};
  function coord_to_string($c) {
  global $r1, $r2;
  return 'Coord3D(x=' . _str($c['x']) . ', y=' . _str($c['y']) . ', z=' . _str($c['z']) . ')';
};
  $r1 = center_of_mass([['x' => 1.5, 'y' => 4.0, 'z' => 3.4, 'mass' => 4.0], ['x' => 5.0, 'y' => 6.8, 'z' => 7.0, 'mass' => 8.1], ['x' => 9.4, 'y' => 10.1, 'z' => 11.6, 'mass' => 12.0]]);
  echo rtrim(coord_to_string($r1)), PHP_EOL;
  $r2 = center_of_mass([['x' => 1.0, 'y' => 2.0, 'z' => 3.0, 'mass' => 4.0], ['x' => 5.0, 'y' => 6.0, 'z' => 7.0, 'mass' => 8.0], ['x' => 9.0, 'y' => 10.0, 'z' => 11.0, 'mass' => 12.0]]);
  echo rtrim(coord_to_string($r2)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
