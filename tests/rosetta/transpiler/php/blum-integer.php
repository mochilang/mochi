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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
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
  function isPrime($n) {
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
};
  function firstPrimeFactor($n) {
  if ($n == 1) {
  return 1;
}
  if ($n % 3 == 0) {
  return 3;
}
  if ($n % 5 == 0) {
  return 5;
}
  $inc = [4, 2, 4, 2, 4, 6, 2, 6];
  $k = 7;
  $i = 0;
  while ($k * $k <= $n) {
  if ($n % $k == 0) {
  return $k;
}
  $k = $k + $inc[$i];
  $i = fmod(($i + 1), count($inc));
};
  return $n;
};
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function padLeft($n, $width) {
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function formatFloat($f, $prec) {
  $s = _str($f);
  $idx = _indexof($s, '.');
  if ($idx < 0) {
  return $s;
}
  $need = $idx + 1 + $prec;
  if (strlen($s) > $need) {
  return substr($s, 0, $need - 0);
}
  return $s;
};
  function main() {
  $blum = [];
  $counts = [0, 0, 0, 0];
  $digits = [1, 3, 7, 9];
  $i = 1;
  $bc = 0;
  while (true) {
  $p = firstPrimeFactor($i);
  if ($p % 4 == 3) {
  $q = intval((_intdiv($i, $p)));
  if ($q != $p && $q % 4 == 3 && isPrime($q)) {
  if ($bc < 50) {
  $blum = array_merge($blum, [$i]);
};
  $d = $i % 10;
  if ($d == 1) {
  $counts[0] = $counts[0] + 1;
} else {
  if ($d == 3) {
  $counts[1] = $counts[1] + 1;
} else {
  if ($d == 7) {
  $counts[2] = $counts[2] + 1;
} else {
  if ($d == 9) {
  $counts[3] = $counts[3] + 1;
};
};
};
};
  $bc = $bc + 1;
  if ($bc == 50) {
  echo rtrim('First 50 Blum integers:'), PHP_EOL;
  $idx = 0;
  while ($idx < 50) {
  $line = '';
  $j = 0;
  while ($j < 10) {
  $line = $line . padLeft($blum[$idx], 3) . ' ';
  $idx = $idx + 1;
  $j = $j + 1;
};
  echo rtrim(json_encode(substr($line, 0, strlen($line) - 1 - 0), 1344)), PHP_EOL;
};
  break;
};
};
}
  if ($i % 5 == 3) {
  $i = $i + 4;
} else {
  $i = $i + 2;
}
};
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
