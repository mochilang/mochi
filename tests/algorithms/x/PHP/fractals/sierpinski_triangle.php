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
function get_mid($p1, $p2) {
  return ['x' => ($p1['x'] + $p2['x']) / 2, 'y' => ($p1['y'] + $p2['y']) / 2];
}
function point_to_string($p) {
  return '(' . _str($p['x']) . ',' . _str($p['y']) . ')';
}
function triangle($v1, $v2, $v3, $depth) {
  echo rtrim(point_to_string($v1) . ' ' . point_to_string($v2) . ' ' . point_to_string($v3)), PHP_EOL;
  if ($depth == 0) {
  return;
}
  triangle($v1, get_mid($v1, $v2), get_mid($v1, $v3), $depth - 1);
  triangle($v2, get_mid($v1, $v2), get_mid($v2, $v3), $depth - 1);
  triangle($v3, get_mid($v3, $v2), get_mid($v1, $v3), $depth - 1);
}
triangle(['x' => -175, 'y' => -125], ['x' => 0, 'y' => 175], ['x' => 175, 'y' => -125], 2);
