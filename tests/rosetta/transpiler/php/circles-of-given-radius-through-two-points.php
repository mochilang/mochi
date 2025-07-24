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
function sqrtApprox($x) {
  global $Two, $R0, $Co, $CoR0, $Diam, $Far, $td, $p1, $p2, $r, $res, $c1, $c2, $caseStr;
  $g = $x;
  $i = 0;
  while ($i < 40) {
  $g = ($g + $x / $g) / 2.0;
  $i = $i + 1;
};
  return $g;
}
function mochi_hypot($x, $y) {
  global $Two, $R0, $Co, $CoR0, $Diam, $Far, $td, $p1, $p2, $r, $res, $c1, $c2, $caseStr;
  return sqrtApprox($x * $x + $y * $y);
}
$Two = 'Two circles.';
$R0 = 'R==0.0 does not describe circles.';
$Co = 'Coincident points describe an infinite number of circles.';
$CoR0 = 'Coincident points with r==0.0 describe a degenerate circle.';
$Diam = 'Points form a diameter and describe only a single circle.';
$Far = 'Points too far apart to form circles.';
function circles($p1, $p2, $r) {
  global $Two, $R0, $Co, $CoR0, $Diam, $Far, $td, $res, $c1, $c2, $caseStr;
  if ($p1['x'] == $p2['x'] && $p1['y'] == $p2['y']) {
  if ($r == 0.0) {
  return [$p1, $p1, 'Coincident points with r==0.0 describe a degenerate circle.'];
};
  return [$p1, $p2, 'Coincident points describe an infinite number of circles.'];
}
  if ($r == 0.0) {
  return [$p1, $p2, 'R==0.0 does not describe circles.'];
}
  $dx = $p2['x'] - $p1['x'];
  $dy = $p2['y'] - $p1['y'];
  $q = mochi_hypot($dx, $dy);
  if ($q > 2.0 * $r) {
  return [$p1, $p2, 'Points too far apart to form circles.'];
}
  $m = ['x' => ($p1['x'] + $p2['x']) / 2.0, 'y' => ($p1['y'] + $p2['y']) / 2.0];
  if ($q == 2.0 * $r) {
  return [$m, $m, 'Points form a diameter and describe only a single circle.'];
}
  $d = sqrtApprox($r * $r - $q * $q / 4.0);
  $ox = $d * $dx / $q;
  $oy = $d * $dy / $q;
  return [['x' => $m['x'] - $oy, 'y' => $m['y'] + $ox], ['x' => $m['x'] + $oy, 'y' => $m['y'] - $ox], 'Two circles.'];
}
$td = [[['x' => 0.1234, 'y' => 0.9876], ['x' => 0.8765, 'y' => 0.2345], 2.0], [['x' => 0.0, 'y' => 2.0], ['x' => 0.0, 'y' => 0.0], 1.0], [['x' => 0.1234, 'y' => 0.9876], ['x' => 0.1234, 'y' => 0.9876], 2.0], [['x' => 0.1234, 'y' => 0.9876], ['x' => 0.8765, 'y' => 0.2345], 0.5], [['x' => 0.1234, 'y' => 0.9876], ['x' => 0.1234, 'y' => 0.9876], 0.0]];
foreach ($td as $tc) {
  $p1 = $tc[0];
  $p2 = $tc[1];
  $r = $tc[2];
  echo rtrim('p1:  {' . _str($p1['x']) . ' ' . _str($p1['y']) . '}'), PHP_EOL;
  echo rtrim('p2:  {' . _str($p2['x']) . ' ' . _str($p2['y']) . '}'), PHP_EOL;
  echo rtrim('r:  ' . _str($r)), PHP_EOL;
  $res = circles($p1, $p2, $r);
  $c1 = $res[0];
  $c2 = $res[1];
  $caseStr = $res[2];
  echo rtrim('   ' . $caseStr), PHP_EOL;
  if ($caseStr == 'Points form a diameter and describe only a single circle.' || $caseStr == 'Coincident points with r==0.0 describe a degenerate circle.') {
  echo rtrim('   Center:  {' . _str($c1['x']) . ' ' . _str($c1['y']) . '}'), PHP_EOL;
} else {
  if ($caseStr == 'Two circles.') {
  echo rtrim('   Center 1:  {' . _str($c1['x']) . ' ' . _str($c1['y']) . '}'), PHP_EOL;
  echo rtrim('   Center 2:  {' . _str($c2['x']) . ' ' . _str($c2['y']) . '}'), PHP_EOL;
};
}
  echo rtrim(''), PHP_EOL;
}
