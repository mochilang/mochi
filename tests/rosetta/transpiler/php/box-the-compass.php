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
function padLeft($s, $w) {
  global $padRight, $indexOf, $format2, $cpx, $compassPoint, $degrees2compasspoint, $headings, $i, $h, $idx, $cp;
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
}
function padRight($s, $w) {
  global $padLeft, $indexOf, $format2, $cpx, $compassPoint, $degrees2compasspoint, $headings, $h, $idx, $cp;
  $out = $s;
  $i = strlen($s);
  while ($i < $w) {
  $out = $out . ' ';
  $i = $i + 1;
};
  return $out;
}
function indexOf($s, $ch) {
  global $padLeft, $padRight, $format2, $cpx, $compassPoint, $degrees2compasspoint, $headings, $h, $idx, $cp;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function format2($f) {
  global $padLeft, $padRight, $indexOf, $cpx, $compassPoint, $degrees2compasspoint, $headings, $i, $h, $cp;
  $s = _str($f);
  $idx = _indexof($s, '.');
  if ($idx < 0) {
  $s = $s . '.00';
} else {
  $need = $idx + 3;
  if (strlen($s) > $need) {
  $s = substr($s, 0, $need - 0);
} else {
  while (strlen($s) < $need) {
  $s = $s . '0';
};
};
}
  return $s;
}
function cpx($h) {
  global $padLeft, $padRight, $indexOf, $format2, $compassPoint, $degrees2compasspoint, $headings, $i, $idx, $cp;
  $x = intval((($h / 11.25) + 0.5));
  $x = $x % 32;
  if ($x < 0) {
  $x = $x + 32;
}
  return $x;
}
$compassPoint = ['North', 'North by east', 'North-northeast', 'Northeast by north', 'Northeast', 'Northeast by east', 'East-northeast', 'East by north', 'East', 'East by south', 'East-southeast', 'Southeast by east', 'Southeast', 'Southeast by south', 'South-southeast', 'South by east', 'South', 'South by west', 'South-southwest', 'Southwest by south', 'Southwest', 'Southwest by west', 'West-southwest', 'West by south', 'West', 'West by north', 'West-northwest', 'Northwest by west', 'Northwest', 'Northwest by north', 'North-northwest', 'North by west'];
function degrees2compasspoint($h) {
  global $padLeft, $padRight, $indexOf, $format2, $cpx, $compassPoint, $headings, $i, $idx, $cp;
  return $compassPoint[cpx($h)];
}
$headings = [0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38];
echo rtrim('Index  Compass point         Degree'), PHP_EOL;
$i = 0;
while ($i < count($headings)) {
  $h = $headings[$i];
  $idx = $i % 32 + 1;
  $cp = degrees2compasspoint($h);
  echo rtrim(padLeft(_str($idx), 4) . '   ' . padRight($cp, 19) . ' ' . format2($h) . '°'), PHP_EOL;
  $i = $i + 1;
}
