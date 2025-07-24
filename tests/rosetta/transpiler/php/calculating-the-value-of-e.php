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
$epsilon = 0.000000000000001;
function absf($x) {
  global $epsilon, $pow10, $formatFloat, $factval, $e, $n, $term;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function pow10($n) {
  global $epsilon, $absf, $formatFloat, $factval, $e, $term;
  $r = 1.0;
  $i = 0;
  while ($i < $n) {
  $r = $r * 10.0;
  $i = $i + 1;
};
  return $r;
}
function formatFloat($f, $prec) {
  global $epsilon, $absf, $pow10, $factval, $e, $term;
  $scale = pow10($prec);
  $scaled = ($f * $scale) + 0.5;
  $n = (intval($scaled));
  $digits = _str($n);
  while (strlen($digits) <= $prec) {
  $digits = '0' . $digits;
};
  $intPart = substr($digits, 0, strlen($digits) - $prec - 0);
  $fracPart = substr($digits, strlen($digits) - $prec, strlen($digits) - strlen($digits) - $prec);
  return $intPart . '.' . $fracPart;
}
$factval = 1;
$e = 2.0;
$n = 2;
$term = 1.0;
while (true) {
  $factval = $factval * $n;
  $n = $n + 1;
  $term = 1.0 / (floatval($factval));
  $e = $e + $term;
  if (absf($term) < $epsilon) {
  break;
}
}
echo rtrim('e = ' . formatFloat($e, 15)), PHP_EOL;
