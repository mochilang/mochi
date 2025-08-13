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
  function stringify($fs) {
  global $inter, $sheru, $sheru_comp, $siya, $uni;
  return $fs['name'] . ': [' . _str($fs['left_boundary']) . ', ' . _str($fs['peak']) . ', ' . _str($fs['right_boundary']) . ']';
};
  function max2($a, $b) {
  global $inter, $sheru, $sheru_comp, $siya, $uni;
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function min2($a, $b) {
  global $inter, $sheru, $sheru_comp, $siya, $uni;
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function complement($fs) {
  global $inter, $sheru, $sheru_comp, $siya, $uni;
  return ['name' => '¬' . $fs['name'], 'left_boundary' => 1.0 - $fs['right_boundary'], 'peak' => 1.0 - $fs['left_boundary'], 'right_boundary' => 1.0 - $fs['peak']];
};
  function intersection($a, $b) {
  global $inter, $sheru, $sheru_comp, $siya, $uni;
  return ['name' => $a['name'] . ' ∩ ' . $b['name'], 'left_boundary' => max2($a['left_boundary'], $b['left_boundary']), 'peak' => min2($a['right_boundary'], $b['right_boundary']), 'right_boundary' => ($a['peak'] + $b['peak']) / 2.0];
};
  function union($a, $b) {
  global $inter, $sheru, $sheru_comp, $siya, $uni;
  return ['name' => $a['name'] . ' U ' . $b['name'], 'left_boundary' => min2($a['left_boundary'], $b['left_boundary']), 'peak' => max2($a['right_boundary'], $b['right_boundary']), 'right_boundary' => ($a['peak'] + $b['peak']) / 2.0];
};
  function membership($fs, $x) {
  global $inter, $sheru, $sheru_comp, $siya, $uni;
  if ($x <= $fs['left_boundary'] || $x >= $fs['right_boundary']) {
  return 0.0;
}
  if ($fs['left_boundary'] < $x && $x <= $fs['peak']) {
  return ($x - $fs['left_boundary']) / ($fs['peak'] - $fs['left_boundary']);
}
  if ($fs['peak'] < $x && $x < $fs['right_boundary']) {
  return ($fs['right_boundary'] - $x) / ($fs['right_boundary'] - $fs['peak']);
}
  return 0.0;
};
  $sheru = ['name' => 'Sheru', 'left_boundary' => 0.4, 'peak' => 1.0, 'right_boundary' => 0.6];
  $siya = ['name' => 'Siya', 'left_boundary' => 0.5, 'peak' => 1.0, 'right_boundary' => 0.7];
  echo rtrim(stringify($sheru)), PHP_EOL;
  echo rtrim(stringify($siya)), PHP_EOL;
  $sheru_comp = complement($sheru);
  echo rtrim(stringify($sheru_comp)), PHP_EOL;
  $inter = intersection($siya, $sheru);
  echo rtrim(stringify($inter)), PHP_EOL;
  echo rtrim('Sheru membership 0.5: ' . _str(membership($sheru, 0.5))), PHP_EOL;
  echo rtrim('Sheru membership 0.6: ' . _str(membership($sheru, 0.6))), PHP_EOL;
  $uni = union($siya, $sheru);
  echo rtrim(stringify($uni)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
