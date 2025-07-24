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
function primeFactors($n) {
  $factors = [];
  $x = $n;
  while ($x % 2 == 0) {
  $factors = array_merge($factors, [2]);
  $x = intval((intdiv($x, 2)));
};
  $p = 3;
  while ($p * $p <= $x) {
  while ($x % $p == 0) {
  $factors = array_merge($factors, [$p]);
  $x = intval((intdiv($x, $p)));
};
  $p = $p + 2;
};
  if ($x > 1) {
  $factors = array_merge($factors, [$x]);
}
  return $factors;
}
function commatize($n) {
  $s = _str($n);
  $out = '';
  $i = strlen($s) - 1;
  $c = 0;
  while ($i >= 0) {
  $out = substr($s, $i, $i + 1 - $i) . $out;
  $c = $c + 1;
  if ($c % 3 == 0 && $i > 0) {
  $out = ',' . $out;
}
  $i = $i - 1;
};
  return $out;
}
function indexOf($s, $sub) {
  $i = 0;
  while ($i + strlen($sub) <= strlen($s)) {
  if (substr($s, $i, $i + strlen($sub) - $i) == $sub) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function pad10($s) {
  $str = $s;
  while (strlen($str) < 10) {
  $str = ' ' . $str;
};
  return $str;
}
function trimRightStr($s) {
  $end = strlen($s);
  while ($end > 0 && substr($s, $end - 1, $end - $end - 1) == ' ') {
  $end = $end - 1;
};
  return substr($s, 0, $end - 0);
}
function main() {
  $res = [];
  $count = 0;
  $k = 11 * 11;
  while ($count < 20) {
  if ($k % 3 == 0 || $k % 5 == 0 || $k % 7 == 0) {
  $k = $k + 2;
  continue;
}
  $factors = primeFactors($k);
  if (count($factors) > 1) {
  $s = _str($k);
  $includesAll = true;
  $prev = -1;
  foreach ($factors as $f) {
  if ($f == $prev) {
  continue;
}
  $fs = _str($f);
  if (_indexof($s, $fs) == (-1)) {
  $includesAll = false;
  break;
}
  $prev = $f;
};
  if ($includesAll) {
  $res = array_merge($res, [$k]);
  $count = $count + 1;
};
}
  $k = $k + 2;
};
  $line = '';
  foreach (array_slice($res, 0, 10 - 0) as $e) {
  $line = $line . pad10(commatize($e)) . ' ';
};
  echo rtrim(trimRightStr($line)), PHP_EOL;
  $line = '';
  foreach (array_slice($res, 10, 20 - 10) as $e) {
  $line = $line . pad10(commatize($e)) . ' ';
};
  echo rtrim(trimRightStr($line)), PHP_EOL;
}
main();
