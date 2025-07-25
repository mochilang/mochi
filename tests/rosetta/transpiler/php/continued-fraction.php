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
function newTerm($a, $b) {
  return ['a' => $a, 'b' => $b];
}
function cfSqrt2($nTerms) {
  $f = [];
  $n = 0;
  while ($n < $nTerms) {
  $f = array_merge($f, [newTerm(2, 1)]);
  $n = $n + 1;
};
  if ($nTerms > 0) {
  $f[0]['a'] = 1;
}
  return $f;
}
function cfNap($nTerms) {
  $f = [];
  $n = 0;
  while ($n < $nTerms) {
  $f = array_merge($f, [newTerm($n, $n - 1)]);
  $n = $n + 1;
};
  if ($nTerms > 0) {
  $f[0]['a'] = 2;
}
  if ($nTerms > 1) {
  $f[1]['b'] = 1;
}
  return $f;
}
function cfPi($nTerms) {
  $f = [];
  $n = 0;
  while ($n < $nTerms) {
  $g = 2 * $n - 1;
  $f = array_merge($f, [newTerm(6, $g * $g)]);
  $n = $n + 1;
};
  if ($nTerms > 0) {
  $f[0]['a'] = 3;
}
  return $f;
}
function real($f) {
  $r = 0.0;
  $i = count($f) - 1;
  while ($i > 0) {
  $r = (floatval($f[$i]['b'])) / ((floatval($f[$i]['a'])) + $r);
  $i = $i - 1;
};
  if (count($f) > 0) {
  $r = $r + (floatval($f[0]['a']));
}
  return $r;
}
function main() {
  echo rtrim('sqrt2: ' . _str(real(cfSqrt2(20)))), PHP_EOL;
  echo rtrim('nap:   ' . _str(real(cfNap(20)))), PHP_EOL;
  echo rtrim('pi:    ' . _str(real(cfPi(20)))), PHP_EOL;
}
main();
