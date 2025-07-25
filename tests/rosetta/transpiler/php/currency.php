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
function parseIntDigits($s) {
  $n = 0;
  $i = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if (!(isset($digits[$ch]))) {
  return 0;
}
  $n = $n * 10 + $digits[$ch];
  $i = $i + 1;
};
  return $n;
}
function parseDC($s) {
  $neg = false;
  if (strlen($s) > 0 && substr($s, 0, 1 - 0) == '-') {
  $neg = true;
  $s = substr($s, 1, strlen($s) - 1);
}
  $dollars = 0;
  $cents = 0;
  $i = 0;
  $seenDot = false;
  $centDigits = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '.') {
  $seenDot = true;
  $i = $i + 1;
  continue;
}
  $d = parseIntDigits($ch);
  if ($seenDot) {
  if ($centDigits < 2) {
  $cents = $cents * 10 + $d;
  $centDigits = $centDigits + 1;
};
} else {
  $dollars = $dollars * 10 + $d;
}
  $i = $i + 1;
};
  if ($centDigits == 1) {
  $cents = $cents * 10;
}
  $val = $dollars * 100 + $cents;
  if ($neg) {
  $val = -$val;
}
  return $val;
}
function parseRate($s) {
  $neg = false;
  if (strlen($s) > 0 && substr($s, 0, 1 - 0) == '-') {
  $neg = true;
  $s = substr($s, 1, strlen($s) - 1);
}
  $whole = 0;
  $frac = 0;
  $digits = 0;
  $seenDot = false;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '.') {
  $seenDot = true;
  $i = $i + 1;
  continue;
}
  $d = parseIntDigits($ch);
  if ($seenDot) {
  if ($digits < 4) {
  $frac = $frac * 10 + $d;
  $digits = $digits + 1;
};
} else {
  $whole = $whole * 10 + $d;
}
  $i = $i + 1;
};
  while ($digits < 4) {
  $frac = $frac * 10;
  $digits = $digits + 1;
};
  $val = $whole * 10000 + $frac;
  if ($neg) {
  $val = -$val;
}
  return $val;
}
function dcString($dc) {
  $d = _intdiv($dc, 100);
  $n = $dc;
  if ($n < 0) {
  $n = -$n;
}
  $c = $n % 100;
  $cstr = _str($c);
  if (strlen($cstr) == 1) {
  $cstr = '0' . $cstr;
}
  return _str($d) . '.' . $cstr;
}
function extend($dc, $n) {
  return $dc * $n;
}
function tax($total, $rate) {
  return intval((_intdiv(($total * $rate + 5000), 10000)));
}
function padLeft($s, $n) {
  $out = $s;
  while (strlen($out) < $n) {
  $out = ' ' . $out;
};
  return $out;
}
function main() {
  $hp = parseDC('5.50');
  $mp = parseDC('2.86');
  $rate = parseRate('0.0765');
  $totalBeforeTax = extend($hp, 4000000000000000) + extend($mp, 2);
  $t = tax($totalBeforeTax, $rate);
  $total = $totalBeforeTax + $t;
  echo rtrim('Total before tax: ' . padLeft(dcString($totalBeforeTax), 22)), PHP_EOL;
  echo rtrim('             Tax: ' . padLeft(dcString($t), 22)), PHP_EOL;
  echo rtrim('           Total: ' . padLeft(dcString($total), 22)), PHP_EOL;
}
main();
