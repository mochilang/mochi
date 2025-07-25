<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $THRESHOLD = 140737488355328;
  function indexOf($xs, $value) {
  global $THRESHOLD;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
};
  function contains($xs, $value) {
  global $THRESHOLD;
  return indexOf($xs, $value) != 0 - 1;
};
  function maxOf($a, $b) {
  global $THRESHOLD;
  if ($a > $b) {
  return $a;
} else {
  return $b;
}
};
  function intSqrt($n) {
  global $THRESHOLD;
  if ($n == 0) {
  return 0;
}
  $x = $n;
  $y = _intdiv(($x + 1), 2);
  while ($y < $x) {
  $x = $y;
  $y = _intdiv(($x + _intdiv($n, $x)), 2);
};
  return $x;
};
  function sumProperDivisors($n) {
  global $THRESHOLD;
  if ($n < 2) {
  return 0;
}
  $sqrt = intSqrt($n);
  $sum = 1;
  $i = 2;
  while ($i <= $sqrt) {
  if ($n % $i == 0) {
  $sum = $sum + $i + _intdiv($n, $i);
}
  $i = $i + 1;
};
  if ($sqrt * $sqrt == $n) {
  $sum = $sum - $sqrt;
}
  return $sum;
};
  function classifySequence($k) {
  global $THRESHOLD;
  $last = $k;
  $seq = [$k];
  while (true) {
  $last = sumProperDivisors($last);
  $seq = array_merge($seq, [$last]);
  $n = count($seq);
  $aliquot = '';
  if ($last == 0) {
  $aliquot = 'Terminating';
} else {
  if ($n == 2 && $last == $k) {
  $aliquot = 'Perfect';
} else {
  if ($n == 3 && $last == $k) {
  $aliquot = 'Amicable';
} else {
  if ($n >= 4 && $last == $k) {
  $aliquot = 'Sociable[' . _str($n - 1) . ']';
} else {
  if ($last == $seq[$n - 2]) {
  $aliquot = 'Aspiring';
} else {
  if (contains(array_slice($seq, 1, maxOf(1, $n - 2) - 1), $last)) {
  $idx = indexOf($seq, $last);
  $aliquot = 'Cyclic[' . _str($n - 1 - $idx) . ']';
} else {
  if ($n == 16 || $last > $THRESHOLD) {
  $aliquot = 'Non-Terminating';
};
};
};
};
};
};
}
  if ($aliquot != '') {
  return ['seq' => $seq, 'aliquot' => $aliquot];
}
};
  return ['seq' => $seq, 'aliquot' => ''];
};
  function padLeft($n, $w) {
  global $THRESHOLD;
  $s = _str($n);
  while (strlen($s) < $w) {
  $s = ' ' . $s;
};
  return $s;
};
  function padRight($s, $w) {
  global $THRESHOLD;
  $r = $s;
  while (strlen($r) < $w) {
  $r = $r . ' ';
};
  return $r;
};
  function joinWithCommas($seq) {
  global $THRESHOLD;
  $s = '[';
  $i = 0;
  while ($i < count($seq)) {
  $s = $s . _str($seq[$i]);
  if ($i < count($seq) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function main() {
  global $THRESHOLD;
  echo rtrim('Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n'), PHP_EOL;
  $k = 1;
  while ($k <= 10) {
  $res = classifySequence($k);
  echo rtrim(padLeft($k, 2) . ': ' . padRight(strval($res['aliquot']), 15) . ' ' . joinWithCommas($res['seq'])), PHP_EOL;
  $k = $k + 1;
};
  echo rtrim(''), PHP_EOL;
  $s = [11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488];
  $i = 0;
  while ($i < count($s)) {
  $val = $s[$i];
  $res = classifySequence($val);
  echo rtrim(padLeft($val, 7) . ': ' . padRight(strval($res['aliquot']), 15) . ' ' . joinWithCommas($res['seq'])), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
  $big = 15355717786080;
  $r = classifySequence($big);
  echo rtrim(_str($big) . ': ' . padRight(strval($r['aliquot']), 15) . ' ' . joinWithCommas($r['seq'])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
