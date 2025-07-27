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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        return bcadd(strval($a), strval($b), 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        return bcsub(strval($a), strval($b), 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        return bcmul(strval($a), strval($b), 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        return intval(bcmod(strval($a), strval($b)));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pad($s, $width) {
  $out = $s;
  while (strlen($out) < $width) {
  $out = ' ' . $out;
};
  return $out;
};
  function mult($n, $base) {
  $m = 1;
  $x = $n;
  $b = $base;
  while ($x > 0) {
  $m = _imul($m, (_imod($x, $b)));
  $x = _idiv($x, $b);
};
  return $m;
};
  function multDigitalRoot($n, $base) {
  $m = $n;
  $mp = 0;
  $b = $base;
  while ($m >= $b) {
  $m = mult($m, $base);
  $mp = $mp + 1;
};
  return ['mp' => $mp, 'mdr' => (intval($m))];
};
  function main() {
  $base = 10;
  $size = 5;
  echo rtrim(pad('Number', 20) . ' ' . pad('MDR', 3) . ' ' . pad('MP', 3)), PHP_EOL;
  $nums = [123321, 7739, 893, 899998, 3778888999, 277777788888899];
  $i = 0;
  while ($i < count($nums)) {
  $n = $nums[$i];
  $r = multDigitalRoot($n, $base);
  echo rtrim(pad(_str($n), 20) . ' ' . pad(_str($r['mdr']), 3) . ' ' . pad(_str($r['mp']), 3)), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
  $list = [];
  $idx = 0;
  while ($idx < $base) {
  $list = array_merge($list, [[]]);
  $idx = $idx + 1;
};
  $cnt = $size * $base;
  $n = 0;
  $b = $base;
  while ($cnt > 0) {
  $r = multDigitalRoot($n, $base);
  $mdr = $r['mdr'];
  if (count($list[$mdr]) < $size) {
  $list[$mdr] = array_merge($list[$mdr], [intval($n)]);
  $cnt = $cnt - 1;
}
  $n = _iadd($n, 1);
};
  echo rtrim('MDR: First'), PHP_EOL;
  $j = 0;
  while ($j < $base) {
  echo rtrim(pad(_str($j), 3) . ': ' . _str($list[$j])), PHP_EOL;
  $j = $j + 1;
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
