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
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $digits = '0123456789abcdef';
  function toBase($n, $b) {
  global $digits, $esths;
  if ($n == 0) {
  return '0';
}
  $v = $n;
  $out = '';
  while ($v > 0) {
  $d = $v % $b;
  $out = substr($digits, $d, $d + 1 - $d) . $out;
  $v = _intdiv($v, $b);
};
  return $out;
};
  function uabs($a, $b) {
  global $digits, $esths;
  if ($a > $b) {
  return $a - $b;
}
  return $b - $a;
};
  function isEsthetic($n, $b) {
  global $digits, $esths;
  if ($n == 0) {
  return false;
}
  $i = $n % $b;
  $n = _intdiv($n, $b);
  while ($n > 0) {
  $j = $n % $b;
  if (uabs($i, $j) != 1) {
  return false;
}
  $n = _intdiv($n, $b);
  $i = $j;
};
  return true;
};
  $esths = [];
  function dfs($n, $m, $i) {
  global $digits, $esths;
  if ($i >= $n && $i <= $m) {
  $esths = array_merge($esths, [$i]);
}
  if ($i == 0 || $i > $m) {
  return;
}
  $d = $i % 10;
  $i1 = $i * 10 + $d - 1;
  $i2 = $i1 + 2;
  if ($d == 0) {
  dfs($n, $m, $i2);
} else {
  if ($d == 9) {
  dfs($n, $m, $i1);
} else {
  dfs($n, $m, $i1);
  dfs($n, $m, $i2);
};
}
};
  function commatize($n) {
  global $digits, $esths;
  $s = _str($n);
  $i = strlen($s) - 3;
  while ($i >= 1) {
  $s = substr($s, 0, $i - 0) . ',' . substr($s, $i, strlen($s) - $i);
  $i = $i - 3;
};
  return $s;
};
  function listEsths($n, $n2, $m, $m2, $perLine, $showAll) {
  global $digits, $esths;
  $esths = [];
  $i = 0;
  while ($i < 10) {
  dfs($n2, $m2, $i);
  $i = $i + 1;
};
  $le = count($esths);
  echo rtrim('Base 10: ' . commatize($le) . ' esthetic numbers between ' . commatize($n) . ' and ' . commatize($m) . ':'), PHP_EOL;
  if ($showAll) {
  $c = 0;
  $line = '';
  foreach ($esths as $v) {
  if (strlen($line) > 0) {
  $line = $line . ' ';
}
  $line = $line . _str($v);
  $c = $c + 1;
  if ($c % $perLine == 0) {
  echo rtrim($line), PHP_EOL;
  $line = '';
}
};
  if (strlen($line) > 0) {
  echo rtrim($line), PHP_EOL;
};
} else {
  $line = '';
  $idx = 0;
  while ($idx < $perLine) {
  if (strlen($line) > 0) {
  $line = $line . ' ';
}
  $line = $line . _str($esths[$idx]);
  $idx = $idx + 1;
};
  echo rtrim($line), PHP_EOL;
  echo rtrim('............'), PHP_EOL;
  $line = '';
  $idx = $le - $perLine;
  while ($idx < $le) {
  if (strlen($line) > 0) {
  $line = $line . ' ';
}
  $line = $line . _str($esths[$idx]);
  $idx = $idx + 1;
};
  echo rtrim($line), PHP_EOL;
}
  echo rtrim(''), PHP_EOL;
};
  function main() {
  global $digits, $esths;
  $b = 2;
  while ($b <= 16) {
  $start = 4 * $b;
  $stop = 6 * $b;
  echo rtrim('Base ' . _str($b) . ': ' . _str($start) . 'th to ' . _str($stop) . 'th esthetic numbers:'), PHP_EOL;
  $n = 1;
  $c = 0;
  $line = '';
  while ($c < $stop) {
  if (isEsthetic($n, $b)) {
  $c = $c + 1;
  if ($c >= $start) {
  if (strlen($line) > 0) {
  $line = $line . ' ';
};
  $line = $line . toBase($n, $b);
};
}
  $n = $n + 1;
};
  echo rtrim($line), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $b = $b + 1;
};
  listEsths(1000, 1010, 9999, 9898, 16, true);
  listEsths(100000000, 101010101, 130000000, 123456789, 9, true);
  listEsths(100000000000, 101010101010, 130000000000, 123456789898, 7, false);
  listEsths(100000000000000, 101010101010101, 130000000000000, 123456789898989, 5, false);
  listEsths(100000000000000000, 101010101010101010, 130000000000000000, 123456789898989898, 4, false);
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
