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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function gcd($a, $b) {
  $x = $a;
  if ($x < 0) {
  $x = -$x;
}
  $y = $b;
  if ($y < 0) {
  $y = -$y;
}
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  return $x;
}
function parseRational($s) {
  $intPart = 0;
  $fracPart = 0;
  $denom = 1;
  $afterDot = false;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '.') {
  $afterDot = true;
} else {
  $d = intval($ch) - intval('0');
  if (!$afterDot) {
  $intPart = $intPart * 10 + $d;
} else {
  $fracPart = $fracPart * 10 + $d;
  $denom = $denom * 10;
};
}
  $i = $i + 1;
};
  $num = $intPart * $denom + $fracPart;
  $g = gcd($num, $denom);
  return ['num' => intval((_intdiv($num, $g))), 'den' => intval((_intdiv($denom, $g)))];
}
function main() {
  $inputs = ['0.9054054', '0.518518', '0.75'];
  foreach ($inputs as $s) {
  $r = parseRational($s);
  echo rtrim($s . ' = ' . _str($r['num']) . '/' . _str($r['den'])), PHP_EOL;
};
}
main();
