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
  function trimLeftZeros($s) {
  $i = 0;
  while ($i < strlen($s) && substr($s, $i, $i + 1 - $i) == '0') {
  $i = $i + 1;
};
  return substr($s, $i, strlen($s) - $i);
};
  function btString($s) {
  $s = trimLeftZeros($s);
  $b = [];
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '+') {
  $b = array_merge($b, [1]);
} else {
  if ($ch == '0') {
  $b = array_merge($b, [0]);
} else {
  if ($ch == '-') {
  $b = array_merge($b, [0 - 1]);
} else {
  return ['bt' => [], 'ok' => false];
};
};
}
  $i = $i - 1;
};
  return ['bt' => $b, 'ok' => true];
};
  function btToString($b) {
  if (count($b) == 0) {
  return '0';
}
  $r = '';
  $i = count($b) - 1;
  while ($i >= 0) {
  $d = $b[$i];
  if ($d == 0 - 1) {
  $r = $r . '-';
} else {
  if ($d == 0) {
  $r = $r . '0';
} else {
  $r = $r . '+';
};
}
  $i = $i - 1;
};
  return $r;
};
  function btInt($i) {
  if ($i == 0) {
  return [];
}
  $n = $i;
  $b = [];
  while ($n != 0) {
  $m = $n % 3;
  $n = intval((_intdiv($n, 3)));
  if ($m == 2) {
  $m = 0 - 1;
  $n = $n + 1;
} else {
  if ($m == 0 - 2) {
  $m = 1;
  $n = $n - 1;
};
}
  $b = array_merge($b, [$m]);
};
  return $b;
};
  function btToInt($b) {
  $r = 0;
  $pt = 1;
  $i = 0;
  while ($i < count($b)) {
  $r = $r + $b[$i] * $pt;
  $pt = $pt * 3;
  $i = $i + 1;
};
  return $r;
};
  function btNeg($b) {
  $r = [];
  $i = 0;
  while ($i < count($b)) {
  $r = array_merge($r, [-$b[$i]]);
  $i = $i + 1;
};
  return $r;
};
  function btAdd($a, $b) {
  return btInt(btToInt($a) + btToInt($b));
};
  function btMul($a, $b) {
  return btInt(btToInt($a) * btToInt($b));
};
  function padLeft($s, $w) {
  $r = $s;
  while (strlen($r) < $w) {
  $r = ' ' . $r;
};
  return $r;
};
  function show($label, $b) {
  $l = padLeft($label, 7);
  $bs = padLeft(btToString($b), 12);
  $is = padLeft(_str(btToInt($b)), 7);
  echo rtrim($l . ' ' . $bs . ' ' . $is), PHP_EOL;
};
  function main() {
  $ares = btString('+-0++0+');
  $a = $ares['bt'];
  $b = btInt(-436);
  $cres = btString('+-++-');
  $c = $cres['bt'];
  show('a:', $a);
  show('b:', $b);
  show('c:', $c);
  show('a(b-c):', btMul($a, btAdd($b, btNeg($c))));
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
