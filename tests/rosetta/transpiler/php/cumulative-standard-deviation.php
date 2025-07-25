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
  if ($x <= 0.0) {
  return 0.0;
}
  $g = $x;
  $i = 0;
  while ($i < 20) {
  $g = ($g + $x / $g) / 2.0;
  $i = $i + 1;
};
  return $g;
}
function newRsdv() {
  return ['n' => 0.0, 'a' => 0.0, 'q' => 0.0];
}
function add($r, $x) {
  $n1 = $r['n'] + 1.0;
  $a1 = $r['a'] + ($x - $r['a']) / $n1;
  $q1 = $r['q'] + ($x - $r['a']) * ($x - $a1);
  return ['n' => $n1, 'a' => $a1, 'q' => $q1];
}
function sd($r) {
  return sqrtApprox($r['q'] / $r['n']);
}
function main() {
  $r = newRsdv();
  foreach ([2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0] as $x) {
  $r = add($r, $x);
  echo rtrim(json_encode(_str(sd($r)), 1344)), PHP_EOL;
};
}
main();
