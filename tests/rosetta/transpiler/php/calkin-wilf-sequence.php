<?php
ini_set('memory_limit', '-1');
function _gcd($a, $b) {
    if (function_exists('bcadd')) {
        if (bccomp($a, '0') < 0) $a = bcsub('0', $a);
        if (bccomp($b, '0') < 0) $b = bcsub('0', $b);
        while (bccomp($b, '0') != 0) {
            $t = bcmod($a, $b);
            $a = $b;
            $b = $t;
        }
        return $a;
    }
    $a = abs($a);
    $b = abs($b);
    while ($b != 0) {
        $t = $a % $b;
        $a = $b;
        $b = $t;
    }
    return $a;
}
function _bigrat($n, $d = 1) {
    if (is_array($n) && isset($n['num']) && isset($n['den']) && $d === null) {
        return $n;
    }
    if ($d === null) { $d = 1; }
    if (function_exists('bcadd')) {
        $n = (string)$n; $d = (string)$d;
        if (bccomp($d, '0') < 0) { $n = bcsub('0', $n); $d = bcsub('0', $d); }
        $g = _gcd($n, $d);
        return ['num' => bcdiv($n, $g, 0), 'den' => bcdiv($d, $g, 0)];
    }
    if ($d < 0) { $n = -$n; $d = -$d; }
    $g = _gcd($n, $d);
    return ['num' => intdiv($n, $g), 'den' => intdiv($d, $g)];
}
function _add($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcadd(bcmul(num($a), denom($b)), bcmul(num($b), denom($a)));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b) + num($b) * denom($a), denom($a) * denom($b));
}
function _sub($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcsub(bcmul(num($a), denom($b)), bcmul(num($b), denom($a)));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b) - num($b) * denom($a), denom($a) * denom($b));
}
function _mul($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcmul(num($a), num($b));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * num($b), denom($a) * denom($b));
}
function _div($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcmul(num($a), denom($b));
        $d = bcmul(denom($a), num($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b), denom($a) * num($b));
}
function num($x) {
    if (is_array($x) && array_key_exists('num', $x)) return $x['num'];
    return $x;
}
function denom($x) {
    if (is_array($x) && array_key_exists('den', $x)) return $x['den'];
    return function_exists('bcadd') ? '1' : 1;
}
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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function bigrat($a, $b) {
  global $calkinWilf, $toContinued, $termNumber, $commatize, $main;
  return _div((_bigrat($a)), (_bigrat($b)));
}
function calkinWilf($n) {
  global $bigrat, $toContinued, $termNumber, $commatize, $main;
  $seq = [];
  $seq = array_merge($seq, [bigrat(1, 1)]);
  $i = 1;
  while ($i < $n) {
  $prev = $seq[$i - 1];
  $a = num($prev);
  $b = denom($prev);
  $f = $a / $b;
  $t = bigrat($f, 1);
  $t = _mul($t, (_bigrat(2)));
  $t = _sub($t, $prev);
  $t = _add($t, (_bigrat(1)));
  $t = _div((_bigrat(1)), $t);
  $seq = array_merge($seq, [$t]);
  $i = $i + 1;
};
  return $seq;
}
function toContinued($r) {
  global $bigrat, $calkinWilf, $termNumber, $commatize, $main;
  $a = num($r);
  $b = denom($r);
  $res = [];
  while (true) {
  $res = array_merge($res, [intval(($a / $b))]);
  $t = $a % $b;
  $a = $b;
  $b = $t;
  if ($a == 1) {
  break;
}
};
  if (count($res) % 2 == 0) {
  $res[count($res) - 1] = $res[count($res) - 1] - 1;
  $res = array_merge($res, [1]);
}
  return $res;
}
function termNumber($cf) {
  global $bigrat, $calkinWilf, $toContinued, $commatize, $main;
  $b = '';
  $d = '1';
  foreach ($cf as $n) {
  $b = repeat($d, $n) . $b;
  if ($d == '1') {
  $d = '0';
} else {
  $d = '1';
}
};
  return parseIntStr($b, 2);
}
function commatize($n) {
  global $bigrat, $calkinWilf, $toContinued, $termNumber, $main;
  $s = _str($n);
  $out = '';
  $i = 0;
  $cnt = 0;
  $neg = false;
  if (substr($s, 0, 1 - 0) == '-') {
  $neg = true;
  $s = substr($s, 1);
}
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $out = substr($s, $i, $i + 1 - $i) . $out;
  $cnt = $cnt + 1;
  if ($cnt == 3 && $i != 0) {
  $out = ',' . $out;
  $cnt = 0;
}
  $i = $i - 1;
};
  if ($neg) {
  $out = '-' . $out;
}
  return $out;
}
function main() {
  global $bigrat, $calkinWilf, $toContinued, $termNumber, $commatize;
  $cw = calkinWilf(20);
  echo rtrim('The first 20 terms of the Calkin-Wilf sequnence are:'), PHP_EOL;
  $i = 0;
  while ($i < 20) {
  $r = $cw[$i];
  $s = _str(num($r));
  if (denom($r) != 1) {
  $s = $s . '/' . _str(denom($r));
}
  echo rtrim(str_pad(($i + intval(1)), 2, ' ', STR_PAD_LEFT) . ': ' . $s), PHP_EOL;
  $i = $i + 1;
};
  $r = bigrat(83116, 51639);
  $cf = toContinued($r);
  $tn = termNumber($cf);
  echo rtrim('' . _str(num($r)) . '/' . _str(denom($r)) . ' is the ' . commatize($tn) . 'th term of the sequence.'), PHP_EOL;
}
main();
