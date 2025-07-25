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
  function nextRand($seed) {
  return ($seed * 1664525 + 1013904223) % 2147483647;
};
  function shuffleChars($s, $seed) {
  $chars = [];
  $i = 0;
  while ($i < strlen($s)) {
  $chars = array_merge($chars, [substr($s, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $sd = $seed;
  $idx = count($chars) - 1;
  while ($idx > 0) {
  $sd = nextRand($sd);
  $j = $sd % ($idx + 1);
  $tmp = $chars[$idx];
  $chars[$idx] = $chars[$j];
  $chars[$j] = $tmp;
  $idx = $idx - 1;
};
  $res = '';
  $i = 0;
  while ($i < count($chars)) {
  $res = $res . $chars[$i];
  $i = $i + 1;
};
  return [$res, $sd];
};
  function bestShuffle($s, $seed) {
  $r = shuffleChars($s, $seed);
  $t = $r[0];
  $sd = $r[1];
  $arr = [];
  $i = 0;
  while ($i < _len($t)) {
  $arr = array_merge($arr, [substr($t, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($arr)) {
  $j = 0;
  while ($j < count($arr)) {
  if ($i != $j && $arr[$i] != substr($s, $j, $j + 1 - $j) && $arr[$j] != substr($s, $i, $i + 1 - $i)) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
  break;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $count = 0;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == substr($s, $i, $i + 1 - $i)) {
  $count = $count + 1;
}
  $i = $i + 1;
};
  $out = '';
  $i = 0;
  while ($i < count($arr)) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return [$out, $sd, $count];
};
  function main() {
  $ts = ['abracadabra', 'seesaw', 'elk', 'grrrrrr', 'up', 'a'];
  $seed = 1;
  $i = 0;
  while ($i < count($ts)) {
  $r = bestShuffle($ts[$i], $seed);
  $shuf = $r[0];
  $seed = $r[1];
  $cnt = $r[2];
  echo rtrim($ts[$i] . ' -> ' . $shuf . ' (' . _str($cnt) . ')'), PHP_EOL;
  $i = $i + 1;
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
