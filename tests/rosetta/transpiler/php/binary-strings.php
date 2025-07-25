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
$__start_mem = memory_get_usage();
$__start = _now();
  function char($n) {
  global $b, $c, $d, $i, $z, $sub, $f, $val, $rem;
  $letters = 'abcdefghijklmnopqrstuvwxyz';
  $idx = $n - 97;
  if ($idx < 0 || $idx >= strlen($letters)) {
  return '?';
}
  return substr($letters, $idx, $idx + 1 - $idx);
};
  function fromBytes($bs) {
  global $b, $c, $d, $z, $sub, $f, $val, $rem;
  $s = '';
  $i = 0;
  while ($i < count($bs)) {
  $s = $s . char($bs[$i]);
  $i = $i + 1;
};
  return $s;
};
  $b = [98, 105, 110, 97, 114, 121];
  echo rtrim(_str($b)), PHP_EOL;
  $c = $b;
  echo rtrim(_str($c)), PHP_EOL;
  echo rtrim(_str($b == $c)), PHP_EOL;
  $d = [];
  $i = 0;
  while ($i < count($b)) {
  $d = array_merge($d, [$b[$i]]);
  $i = $i + 1;
}
  $d[1] = 97;
  $d[4] = 110;
  echo rtrim(fromBytes($b)), PHP_EOL;
  echo rtrim(fromBytes($d)), PHP_EOL;
  echo rtrim(_str(count($b) == 0)), PHP_EOL;
  $z = array_merge($b, [122]);
  echo rtrim(fromBytes($z)), PHP_EOL;
  $sub = array_slice($b, 1, 3 - 1);
  echo rtrim(fromBytes($sub)), PHP_EOL;
  $f = [];
  $i = 0;
  while ($i < count($d)) {
  $val = $d[$i];
  if ($val == 110) {
  $f = array_merge($f, [109]);
} else {
  $f = array_merge($f, [$val]);
}
  $i = $i + 1;
}
  echo rtrim(fromBytes($d) . ' -> ' . fromBytes($f)), PHP_EOL;
  $rem = [];
  $rem = array_merge($rem, [$b[0]]);
  $i = 3;
  while ($i < count($b)) {
  $rem = array_merge($rem, [$b[$i]]);
  $i = $i + 1;
}
  echo rtrim(fromBytes($rem)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
