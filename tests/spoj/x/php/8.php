<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_split($s, $sep) {
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = _append($parts, $cur);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = _append($parts, $cur);
  return $parts;
};
  function parse_ints($line) {
  $pieces = mochi_split($line, ' ');
  $nums = [];
  foreach ($pieces as $p) {
  if (strlen($p) > 0) {
  $nums = _append($nums, intval($p));
}
};
  return $nums;
};
  function main() {
  $tLine = trim(fgets(STDIN));
  if ($tLine == '') {
  return;
}
  $t = intval($tLine);
  $caseIdx = 0;
  while ($caseIdx < $t) {
  $header = parse_ints(trim(fgets(STDIN)));
  $s = $header[0];
  $c = $header[1];
  $seq = parse_ints(trim(fgets(STDIN)));
  $levels = [];
  $levels = _append($levels, $seq);
  $current = $seq;
  while (count($current) > 1) {
  $next = [];
  $i = 0;
  while ($i + 1 < count($current)) {
  $next = _append($next, $current[$i + 1] - $current[$i]);
  $i = $i + 1;
};
  $levels = _append($levels, $next);
  $current = $next;
};
  $depth = count($levels) - 1;
  $step = 0;
  $res = [];
  while ($step < $c) {
  $bottom = $levels[$depth];
  $bottom = _append($bottom, $bottom[count($bottom) - 1]);
  $levels[$depth] = $bottom;
  $level = $depth - 1;
  while ($level >= 0) {
  $arr = $levels[$level];
  $arrBelow = $levels[$level + 1];
  $nextVal = $arr[count($arr) - 1] + $arrBelow[count($arrBelow) - 1];
  $arr = _append($arr, $nextVal);
  $levels[$level] = $arr;
  $level = $level - 1;
};
  $res = _append($res, $levels[0][count($levels[0]) - 1]);
  $step = $step + 1;
};
  $out = '';
  $i2 = 0;
  while ($i2 < count($res)) {
  if ($i2 > 0) {
  $out = $out . ' ';
}
  $out = $out . _str($res[$i2]);
  $i2 = $i2 + 1;
};
  echo rtrim($out), PHP_EOL;
  $caseIdx = $caseIdx + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
