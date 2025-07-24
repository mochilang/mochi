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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function indexOf($s, $ch) {
  global $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function fmt4($x) {
  global $indexOf, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $y = $x * 10000.0;
  if ($y >= 0) {
  $y = $y + 0.5;
} else {
  $y = $y - 0.5;
}
  $y = floatval((intval($y))) / 10000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.0000';
} else {
  $decs = strlen($s) - $dot - 1;
  if ($decs > 4) {
  $s = substr($s, 0, $dot + 5 - 0);
} else {
  while ($decs < 4) {
  $s = $s . '0';
  $decs = $decs + 1;
};
};
}
  if ($x >= 0.0) {
  $s = ' ' . $s;
}
  return $s;
}
function fmt2($n) {
  global $indexOf, $fmt4, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $s = _str($n);
  if (strlen($s) < 2) {
  return ' ' . $s;
}
  return $s;
}
function sumPoint($p1, $p2) {
  global $indexOf, $fmt4, $fmt2, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  return ['x' => $p1['x'] + $p2['x'], 'y' => $p1['y'] + $p2['y'], 'z' => $p1['z'] + $p2['z']];
}
function mulPoint($p, $m) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  return ['x' => $p['x'] * $m, 'y' => $p['y'] * $m, 'z' => $p['z'] * $m];
}
function divPoint($p, $d) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  return mulPoint($p, 1.0 / $d);
}
function centerPoint($p1, $p2) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  return divPoint(sumPoint($p1, $p2), 2.0);
}
function getFacePoints($points, $faces) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $facePoints = [];
  $i = 0;
  while ($i < count($faces)) {
  $face = $faces[$i];
  $fp = ['x' => 0.0, 'y' => 0.0, 'z' => 0.0];
  foreach ($face as $idx) {
  $fp = sumPoint($fp, $points[$idx]);
};
  $fp = divPoint($fp, (floatval(count($face))));
  $facePoints = array_merge($facePoints, [$fp]);
  $i = $i + 1;
};
  return $facePoints;
}
function sortEdges($edges) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $res = [];
  $tmp = $edges;
  while (count($tmp) > 0) {
  $min = $tmp[0];
  $idx = 0;
  $j = 1;
  while ($j < count($tmp)) {
  $e = $tmp[$j];
  if ($e[0] < $min[0] || ($e[0] == $min[0] && ($e[1] < $min[1] || ($e[1] == $min[1] && $e[2] < $min[2])))) {
  $min = $e;
  $idx = $j;
}
  $j = $j + 1;
};
  $res = array_merge($res, [$min]);
  $out = [];
  $k = 0;
  while ($k < count($tmp)) {
  if ($k != $idx) {
  $out = array_merge($out, [$tmp[$k]]);
}
  $k = $k + 1;
};
  $tmp = $out;
};
  return $res;
}
function getEdgesFaces($points, $faces) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $edges = [];
  $fnum = 0;
  while ($fnum < count($faces)) {
  $face = $faces[$fnum];
  $numP = count($face);
  $pi = 0;
  while ($pi < $numP) {
  $pn1 = $face[$pi];
  $pn2 = 0;
  if ($pi < $numP - 1) {
  $pn2 = $face[$pi + 1];
} else {
  $pn2 = $face[0];
}
  if ($pn1 > $pn2) {
  $tmpn = $pn1;
  $pn1 = $pn2;
  $pn2 = $tmpn;
}
  $edges = array_merge($edges, [[$pn1, $pn2, $fnum]]);
  $pi = $pi + 1;
};
  $fnum = $fnum + 1;
};
  $edges = sortEdges($edges);
  $merged = [];
  $idx = 0;
  while ($idx < count($edges)) {
  $e1 = $edges[$idx];
  if ($idx < count($edges) - 1) {
  $e2 = $edges[$idx + 1];
  if ($e1[0] == $e2[0] && $e1[1] == $e2[1]) {
  $merged = array_merge($merged, [[$e1[0], $e1[1], $e1[2], $e2[2]]]);
  $idx = $idx + 2;
  continue;
};
}
  $merged = array_merge($merged, [[$e1[0], $e1[1], $e1[2], -1]]);
  $idx = $idx + 1;
};
  $edgesCenters = [];
  foreach ($merged as $me) {
  $p1 = $points[$me[0]];
  $p2 = $points[$me[1]];
  $cp = centerPoint($p1, $p2);
  $edgesCenters = array_merge($edgesCenters, [['pn1' => $me[0], 'pn2' => $me[1], 'fn1' => $me[2], 'fn2' => $me[3], 'cp' => $cp]]);
};
  return $edgesCenters;
}
function getEdgePoints($points, $edgesFaces, $facePoints) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $edgePoints = [];
  $i = 0;
  while ($i < count($edgesFaces)) {
  $edge = $edgesFaces[$i];
  $cp = $edge['cp'];
  $fp1 = $facePoints[$edge['fn1']];
  $fp2 = $fp1;
  if ($edge['fn2'] != 0 - 1) {
  $fp2 = $facePoints[$edge['fn2']];
}
  $cfp = centerPoint($fp1, $fp2);
  $edgePoints = array_merge($edgePoints, [centerPoint($cp, $cfp)]);
  $i = $i + 1;
};
  return $edgePoints;
}
function getAvgFacePoints($points, $faces, $facePoints) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $numP = count($points);
  $temp = [];
  $i = 0;
  while ($i < $numP) {
  $temp = array_merge($temp, [['p' => ['x' => 0.0, 'y' => 0.0, 'z' => 0.0], 'n' => 0]]);
  $i = $i + 1;
};
  $fnum = 0;
  while ($fnum < count($faces)) {
  $fp = $facePoints[$fnum];
  foreach ($faces[$fnum] as $pn) {
  $tp = $temp[$pn];
  $temp[$pn] = ['p' => sumPoint($tp['p'], $fp), 'n' => $tp['n'] + 1];
};
  $fnum = $fnum + 1;
};
  $avg = [];
  $j = 0;
  while ($j < $numP) {
  $tp = $temp[$j];
  $avg = array_merge($avg, [divPoint($tp['p'], floatval($tp['n']))]);
  $j = $j + 1;
};
  return $avg;
}
function getAvgMidEdges($points, $edgesFaces) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $numP = count($points);
  $temp = [];
  $i = 0;
  while ($i < $numP) {
  $temp = array_merge($temp, [['p' => ['x' => 0.0, 'y' => 0.0, 'z' => 0.0], 'n' => 0]]);
  $i = $i + 1;
};
  foreach ($edgesFaces as $edge) {
  $cp = $edge['cp'];
  $arr = [$edge['pn1'], $edge['pn2']];
  foreach ($arr as $pn) {
  $tp = $temp[$pn];
  $temp[$pn] = ['p' => sumPoint($tp['p'], $cp), 'n' => $tp['n'] + 1];
};
};
  $avg = [];
  $j = 0;
  while ($j < $numP) {
  $tp = $temp[$j];
  $avg = array_merge($avg, [divPoint($tp['p'], floatval($tp['n']))]);
  $j = $j + 1;
};
  return $avg;
}
function getPointsFaces($points, $faces) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $pf = [];
  $i = 0;
  while ($i < count($points)) {
  $pf = array_merge($pf, [0]);
  $i = $i + 1;
};
  $fnum = 0;
  while ($fnum < count($faces)) {
  foreach ($faces[$fnum] as $pn) {
  $pf[$pn] = $pf[$pn] + 1;
};
  $fnum = $fnum + 1;
};
  return $pf;
}
function getNewPoints($points, $pf, $afp, $ame) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace, $main;
  $newPts = [];
  $i = 0;
  while ($i < count($points)) {
  $n = floatval($pf[$i]);
  $m1 = ($n - 3.0) / $n;
  $m2 = 1.0 / $n;
  $m3 = 2.0 / $n;
  $old = $points[$i];
  $p1 = mulPoint($old, $m1);
  $p2 = mulPoint($afp[$i], $m2);
  $p3 = mulPoint($ame[$i], $m3);
  $newPts = array_merge($newPts, [sumPoint(sumPoint($p1, $p2), $p3)]);
  $i = $i + 1;
};
  return $newPts;
}
function mochi_key($a, $b) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $cmcSubdiv, $formatPoint, $formatFace, $main;
  if ($a < $b) {
  return _str($a) . ',' . _str($b);
}
  return _str($b) . ',' . _str($a);
}
function cmcSubdiv($points, $faces) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $formatPoint, $formatFace, $main;
  $facePoints = getFacePoints($points, $faces);
  $edgesFaces = getEdgesFaces($points, $faces);
  $edgePoints = getEdgePoints($points, $edgesFaces, $facePoints);
  $avgFacePoints = getAvgFacePoints($points, $faces, $facePoints);
  $avgMidEdges = getAvgMidEdges($points, $edgesFaces);
  $pointsFaces = getPointsFaces($points, $faces);
  $newPoints = getNewPoints($points, $pointsFaces, $avgFacePoints, $avgMidEdges);
  $facePointNums = [];
  $nextPoint = count($newPoints);
  foreach ($facePoints as $fp) {
  $newPoints = array_merge($newPoints, [$fp]);
  $facePointNums = array_merge($facePointNums, [$nextPoint]);
  $nextPoint = $nextPoint + 1;
};
  $edgePointNums = [];
  $idx = 0;
  while ($idx < count($edgesFaces)) {
  $e = $edgesFaces[$idx];
  $newPoints = array_merge($newPoints, [$edgePoints[$idx]]);
  $edgePointNums[mochi_key($e['pn1'], $e['pn2'])] = $nextPoint;
  $nextPoint = $nextPoint + 1;
  $idx = $idx + 1;
};
  $newFaces = [];
  $fnum = 0;
  while ($fnum < count($faces)) {
  $oldFace = $faces[$fnum];
  if (count($oldFace) == 4) {
  $a = $oldFace[0];
  $b = $oldFace[1];
  $c = $oldFace[2];
  $d = $oldFace[3];
  $fpnum = $facePointNums[$fnum];
  $ab = $edgePointNums[mochi_key($a, $b)];
  $da = $edgePointNums[mochi_key($d, $a)];
  $bc = $edgePointNums[mochi_key($b, $c)];
  $cd = $edgePointNums[mochi_key($c, $d)];
  $newFaces = array_merge($newFaces, [[$a, $ab, $fpnum, $da]]);
  $newFaces = array_merge($newFaces, [[$b, $bc, $fpnum, $ab]]);
  $newFaces = array_merge($newFaces, [[$c, $cd, $fpnum, $bc]]);
  $newFaces = array_merge($newFaces, [[$d, $da, $fpnum, $cd]]);
}
  $fnum = $fnum + 1;
};
  return [$newPoints, $newFaces];
}
function formatPoint($p) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatFace, $main;
  return '[' . fmt4($p['x']) . ' ' . fmt4($p['y']) . ' ' . fmt4($p['z']) . ']';
}
function formatFace($f) {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $main;
  if (count($f) == 0) {
  return '[]';
}
  $s = '[' . fmt2($f[0]);
  $i = 1;
  while ($i < count($f)) {
  $s = $s . ' ' . fmt2($f[$i]);
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function main() {
  global $indexOf, $fmt4, $fmt2, $sumPoint, $mulPoint, $divPoint, $centerPoint, $getFacePoints, $sortEdges, $getEdgesFaces, $getEdgePoints, $getAvgFacePoints, $getAvgMidEdges, $getPointsFaces, $getNewPoints, $mochi_key, $cmcSubdiv, $formatPoint, $formatFace;
  $inputPoints = [['x' => -1.0, 'y' => 1.0, 'z' => 1.0], ['x' => -1.0, 'y' => -1.0, 'z' => 1.0], ['x' => 1.0, 'y' => -1.0, 'z' => 1.0], ['x' => 1.0, 'y' => 1.0, 'z' => 1.0], ['x' => 1.0, 'y' => -1.0, 'z' => -1.0], ['x' => 1.0, 'y' => 1.0, 'z' => -1.0], ['x' => -1.0, 'y' => -1.0, 'z' => -1.0], ['x' => -1.0, 'y' => 1.0, 'z' => -1.0]];
  $inputFaces = [[0, 1, 2, 3], [3, 2, 4, 5], [5, 4, 6, 7], [7, 0, 3, 5], [7, 6, 1, 0], [6, 1, 2, 4]];
  $outputPoints = $inputPoints;
  $outputFaces = $inputFaces;
  $i = 0;
  while ($i < 1) {
  $res = cmcSubdiv($outputPoints, $outputFaces);
  $outputPoints = $res[0];
  $outputFaces = $res[1];
  $i = $i + 1;
};
  foreach ($outputPoints as $p) {
  echo rtrim(formatPoint($p)), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
  foreach ($outputFaces as $f) {
  echo rtrim(formatFace($f)), PHP_EOL;
};
}
main();
