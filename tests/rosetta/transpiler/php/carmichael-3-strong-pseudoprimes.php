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
function mod($n, $m) {
  global $isPrime, $pad, $carmichael;
  return (($n % $m) + $m) % $m;
}
function isPrime($n) {
  global $mod, $pad, $carmichael;
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  if ($n % 3 == 0) {
  return $n == 3;
}
  $d = 5;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 4;
};
  return true;
}
function pad($n, $width) {
  global $mod, $isPrime, $carmichael;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function carmichael($p1) {
  global $mod, $isPrime, $pad;
  for ($h3 = 2; $h3 < $p1; $h3++) {
  for ($d = 1; $d < ($h3 + $p1); $d++) {
  if ((($h3 + $p1) * ($p1 - 1)) % $d == 0 && mod(-$p1 * $p1, $h3) == $d % $h3) {
  $p2 = 1 + (intdiv(($p1 - 1) * ($h3 + $p1), $d));
  if (!isPrime($p2)) {
  continue;
};
  $p3 = 1 + (intdiv($p1 * $p2, $h3));
  if (!isPrime($p3)) {
  continue;
};
  if (($p2 * $p3) % ($p1 - 1) != 1) {
  continue;
};
  $c = $p1 * $p2 * $p3;
  echo rtrim(pad($p1, 2) . '   ' . pad($p2, 4) . '   ' . pad($p3, 5) . '     ' . _str($c)), PHP_EOL;
}
};
};
}
echo rtrim('The following are Carmichael munbers for p1 <= 61:
'), PHP_EOL;
echo rtrim('p1     p2      p3     product'), PHP_EOL;
echo rtrim('==     ==      ==     ======='), PHP_EOL;
for ($p1 = 2; $p1 < 62; $p1++) {
  if (isPrime($p1)) {
  carmichael($p1);
}
}
