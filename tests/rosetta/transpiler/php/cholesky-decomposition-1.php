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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
$__start_mem = memory_get_usage();
$__start = _now();
  function sqrtApprox($x) {
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function makeSym($order, $elements) {
  return ['order' => $order, 'ele' => $elements];
};
  function unpackSym($m) {
  $n = $m['order'];
  $ele = $m['ele'];
  $mat = [];
  $idx = 0;
  $r = 0;
  while ($r < $n) {
  $row = [];
  $c = 0;
  while ($c <= $r) {
  $row = array_merge($row, [$ele[$idx]]);
  $idx = $idx + 1;
  $c = $c + 1;
};
  while ($c < $n) {
  $row = array_merge($row, [0.0]);
  $c = $c + 1;
};
  $mat = array_merge($mat, [$row]);
  $r = $r + 1;
};
  $r = 0;
  while ($r < $n) {
  $c = $r + 1;
  while ($c < $n) {
  $mat[$r][$c] = $mat[$c][$r];
  $c = $c + 1;
};
  $r = $r + 1;
};
  return $mat;
};
  function printMat($m) {
  $i = 0;
  while ($i < count($m)) {
  $line = '';
  $j = 0;
  while ($j < count($m[$i])) {
  $line = $line . _str($m[$i][$j]);
  if ($j < count($m[$i]) - 1) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  function printSym($m) {
  printMat(unpackSym($m));
};
  function printLower($m) {
  $n = $m['order'];
  $ele = $m['ele'];
  $mat = [];
  $idx = 0;
  $r = 0;
  while ($r < $n) {
  $row = [];
  $c = 0;
  while ($c <= $r) {
  $row = array_merge($row, [$ele[$idx]]);
  $idx = $idx + 1;
  $c = $c + 1;
};
  while ($c < $n) {
  $row = array_merge($row, [0.0]);
  $c = $c + 1;
};
  $mat = array_merge($mat, [$row]);
  $r = $r + 1;
};
  printMat($mat);
};
  function choleskyLower($a) {
  $n = $a['order'];
  $ae = $a['ele'];
  $le = [];
  $idx = 0;
  while ($idx < _len($ae)) {
  $le = array_merge($le, [0.0]);
  $idx = $idx + 1;
};
  $row = 1;
  $col = 1;
  $dr = 0;
  $dc = 0;
  $i = 0;
  while ($i < _len($ae)) {
  $e = $ae[$i];
  if ($i < $dr) {
  $d = ($e - $le[$i]) / $le[$dc];
  $le[$i] = $d;
  $ci = $col;
  $cx = $dc;
  $j = $i + 1;
  while ($j <= $dr) {
  $cx = $cx + $ci;
  $ci = $ci + 1;
  $le[$j] = $le[$j] + $d * $le[$cx];
  $j = $j + 1;
};
  $col = $col + 1;
  $dc = $dc + $col;
} else {
  $le[$i] = sqrtApprox($e - $le[$i]);
  $row = $row + 1;
  $dr = $dr + $row;
  $col = 1;
  $dc = 0;
}
  $i = $i + 1;
};
  return ['order' => $n, 'ele' => $le];
};
  function demo($a) {
  echo rtrim('A:'), PHP_EOL;
  printSym($a);
  echo rtrim('L:'), PHP_EOL;
  $l = choleskyLower($a);
  printLower($l);
};
  demo(makeSym(3, [25.0, 15.0, 18.0, -5.0, 0.0, 11.0]));
  demo(makeSym(4, [18.0, 22.0, 70.0, 54.0, 86.0, 174.0, 42.0, 62.0, 134.0, 106.0]));
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
