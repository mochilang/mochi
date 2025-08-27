<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  function mochi_split($s, $sep) {
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = _append($parts, $cur);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = _append($parts, $cur);
  return $parts;
};
  function parse_ints($line) {
  $pieces = mochi_split($line, ' ');
  $nums = [];
  $i = 0;
  while ($i < count($pieces)) {
  $p = $pieces[$i];
  if (strlen($p) > 0) {
  $nums = _append($nums, intval($p));
}
  $i = $i + 1;
};
  return $nums;
};
  function sort_unique(&$arr) {
  $i = 1;
  while ($i < count($arr)) {
  $j = $i;
  while ($j > 0 && $arr[$j - 1] > $arr[$j]) {
  $tmp = $arr[$j - 1];
  $arr[$j - 1] = $arr[$j];
  $arr[$j] = $tmp;
  $j = $j - 1;
};
  $i = $i + 1;
};
  $res = [];
  $i = 0;
  while ($i < count($arr)) {
  if ($i == 0 || $arr[$i] != $arr[$i - 1]) {
  $res = _append($res, $arr[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function pointInPoly($xs, $ys, $px, $py) {
  $inside = false;
  $i = 0;
  $j = count($xs) - 1;
  while ($i < count($xs)) {
  $xi = floatval($xs[$i]);
  $yi = floatval($ys[$i]);
  $xj = floatval($xs[$j]);
  $yj = floatval($ys[$j]);
  if ((($yi > $py) && ($yj <= $py)) || (($yj > $py) && ($yi <= $py))) {
  $xint = ($xj - $xi) * ($py - $yi) / ($yj - $yi) + $xi;
  if ($px < $xint) {
  $inside = !$inside;
};
}
  $j = $i;
  $i = $i + 1;
};
  return $inside;
};
  function make3DBool($a, $b, $c) {
  $arr = [];
  $i = 0;
  while ($i < $a) {
  $plane = [];
  $j = 0;
  while ($j < $b) {
  $row = [];
  $k = 0;
  while ($k < $c) {
  $row = _append($row, false);
  $k = $k + 1;
};
  $plane = _append($plane, $row);
  $j = $j + 1;
};
  $arr = _append($arr, $plane);
  $i = $i + 1;
};
  return $arr;
};
  function main() {
  $tLine = trim(fgets(STDIN));
  if ($tLine == '') {
  return;
}
  $t = intval($tLine);
  $case = 0;
  while ($case < $t) {
  $fLine = trim(fgets(STDIN));
  $F = intval($fLine);
  $xs = [];
  $ys = [];
  $zs = [];
  $xs = _append($xs, 0);
  $xs = _append($xs, 1001);
  $ys = _append($ys, 0);
  $ys = _append($ys, 1001);
  $zs = _append($zs, 0);
  $zs = _append($zs, 1001);
  $faceXCoord = [];
  $faceYPoly = [];
  $faceZPoly = [];
  $i = 0;
  while ($i < $F) {
  $line = trim(fgets(STDIN));
  $nums = parse_ints($line);
  $P = $nums[0];
  $ptsX = [];
  $ptsY = [];
  $ptsZ = [];
  $j = 0;
  while ($j < $P) {
  $x = $nums[1 + 3 * $j];
  $y = $nums[1 + 3 * $j + 1];
  $z = $nums[1 + 3 * $j + 2];
  $ptsX = _append($ptsX, $x);
  $ptsY = _append($ptsY, $y);
  $ptsZ = _append($ptsZ, $z);
  $xs = _append($xs, $x);
  $ys = _append($ys, $y);
  $zs = _append($zs, $z);
  $j = $j + 1;
};
  $allSame = true;
  $j = 1;
  while ($j < $P) {
  if ($ptsX[$j] != $ptsX[0]) {
  $allSame = false;
}
  $j = $j + 1;
};
  if ($allSame) {
  $faceXCoord = _append($faceXCoord, $ptsX[0]);
  $faceYPoly = _append($faceYPoly, $ptsY);
  $faceZPoly = _append($faceZPoly, $ptsZ);
}
  $i = $i + 1;
};
  $xs = sort_unique($xs);
  $ys = sort_unique($ys);
  $zs = sort_unique($zs);
  $nx = count($xs) - 1;
  $ny = count($ys) - 1;
  $nz = count($zs) - 1;
  $xIndex = [];
  $i = 0;
  while ($i < count($xs)) {
  $xIndex[$xs[$i]] = $i;
  $i = $i + 1;
};
  $dx = [];
  $i = 0;
  while ($i < $nx) {
  $dx = _append($dx, $xs[$i + 1] - $xs[$i]);
  $i = $i + 1;
};
  $dy = [];
  $i = 0;
  while ($i < $ny) {
  $dy = _append($dy, $ys[$i + 1] - $ys[$i]);
  $i = $i + 1;
};
  $dz = [];
  $i = 0;
  while ($i < $nz) {
  $dz = _append($dz, $zs[$i + 1] - $zs[$i]);
  $i = $i + 1;
};
  $blockX = make3DBool(count($xs), $ny, $nz);
  $i = 0;
  while ($i < count($faceXCoord)) {
  $coord = $faceXCoord[$i];
  $polyY = $faceYPoly[$i];
  $polyZ = $faceZPoly[$i];
  $xi = $xIndex[$coord];
  $j = 0;
  while ($j < $ny) {
  $cy = (floatval(($ys[$j] + $ys[$j + 1]))) / 2.0;
  $k = 0;
  while ($k < $nz) {
  $cz = (floatval(($zs[$k] + $zs[$k + 1]))) / 2.0;
  if (pointInPoly($polyY, $polyZ, $cy, $cz)) {
  $blockX[$xi][$j][$k] = true;
}
  $k = $k + 1;
};
  $j = $j + 1;
};
  $i = $i + 1;
};
  $solid = make3DBool($nx, $ny, $nz);
  $j2 = 0;
  while ($j2 < $ny) {
  $k2 = 0;
  while ($k2 < $nz) {
  $inside = false;
  $i2 = 0;
  while ($i2 < $nx) {
  if ($blockX[$i2][$j2][$k2]) {
  $inside = !$inside;
}
  if ($inside) {
  $solid[$i2][$j2][$k2] = true;
}
  $i2 = $i2 + 1;
};
  $k2 = $k2 + 1;
};
  $j2 = $j2 + 1;
};
  $volume = 0;
  $i3 = 0;
  while ($i3 < $nx) {
  $j3 = 0;
  while ($j3 < $ny) {
  $k3 = 0;
  while ($k3 < $nz) {
  if ($solid[$i3][$j3][$k3]) {
  $volume = $volume + $dx[$i3] * $dy[$j3] * $dz[$k3];
}
  $k3 = $k3 + 1;
};
  $j3 = $j3 + 1;
};
  $i3 = $i3 + 1;
};
  echo rtrim('The bulk is composed of ' . _str($volume) . ' units.'), PHP_EOL;
  $case = $case + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
