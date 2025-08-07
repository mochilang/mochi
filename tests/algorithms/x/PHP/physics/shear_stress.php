<?php
ini_set('memory_limit', '-1');
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
function shear_stress($stress, $tangential_force, $area) {
  global $r1, $r2, $r3;
  $zeros = 0;
  if ($stress == 0.0) {
  $zeros = $zeros + 1;
}
  if ($tangential_force == 0.0) {
  $zeros = $zeros + 1;
}
  if ($area == 0.0) {
  $zeros = $zeros + 1;
}
  if ($zeros != 1) {
  _panic('You cannot supply more or less than 2 values');
} else {
  if ($stress < 0.0) {
  _panic('Stress cannot be negative');
} else {
  if ($tangential_force < 0.0) {
  _panic('Tangential Force cannot be negative');
} else {
  if ($area < 0.0) {
  _panic('Area cannot be negative');
} else {
  if ($stress == 0.0) {
  return ['name' => 'stress', 'value' => $tangential_force / $area];
} else {
  if ($tangential_force == 0.0) {
  return ['name' => 'tangential_force', 'value' => $stress * $area];
} else {
  return ['name' => 'area', 'value' => $tangential_force / $stress];
};
};
};
};
};
}
}
function str_result($r) {
  global $r1, $r2, $r3;
  return 'Result(name=\'' . $r['name'] . '\', value=' . _str($r['value']) . ')';
}
$r1 = shear_stress(25.0, 100.0, 0.0);
echo rtrim(str_result($r1)), PHP_EOL;
$r2 = shear_stress(0.0, 1600.0, 200.0);
echo rtrim(str_result($r2)), PHP_EOL;
$r3 = shear_stress(1000.0, 0.0, 1200.0);
echo rtrim(str_result($r3)), PHP_EOL;
