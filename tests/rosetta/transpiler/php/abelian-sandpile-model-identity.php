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
  function neighborsList() {
  global $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  return [[1, 3], [0, 2, 4], [1, 5], [0, 4, 6], [1, 3, 5, 7], [2, 4, 8], [3, 7], [4, 6, 8], [5, 7]];
};
  function plus($a, $b) {
  global $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = array_merge($res, [$a[$i] + $b[$i]]);
  $i = $i + 1;
};
  return $res;
};
  function isStable($p) {
  global $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  foreach ($p as $v) {
  if ($v > 3) {
  return false;
}
};
  return true;
};
  function topple(&$p) {
  global $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  $neighbors = neighborsList();
  $i = 0;
  while ($i < count($p)) {
  if ($p[$i] > 3) {
  $p[$i] = $p[$i] - 4;
  $nbs = $neighbors[$i];
  foreach ($nbs as $j) {
  $p[$j] = $p[$j] + 1;
};
  return 0;
}
  $i = $i + 1;
};
  return 0;
};
  function pileString($p) {
  global $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  $s = '';
  $r = 0;
  while ($r < 3) {
  $c = 0;
  while ($c < 3) {
  $s = $s . _str($p[3 * $r + $c]) . ' ';
  $c = $c + 1;
};
  $s = $s . '\n';
  $r = $r + 1;
};
  return $s;
};
  echo rtrim('Avalanche of topplings:\n'), PHP_EOL;
  $s4 = [4, 3, 3, 3, 1, 2, 0, 2, 3];
  echo rtrim(pileString($s4)), PHP_EOL;
  while (!isStable($s4)) {
  topple($s4);
  echo rtrim(pileString($s4)), PHP_EOL;
}
  echo rtrim('Commutative additions:\n'), PHP_EOL;
  $s1 = [1, 2, 0, 2, 1, 1, 0, 1, 3];
  $s2 = [2, 1, 3, 1, 0, 1, 0, 1, 0];
  $s3_a = plus($s1, $s2);
  while (!isStable($s3_a)) {
  topple($s3_a);
}
  $s3_b = plus($s2, $s1);
  while (!isStable($s3_b)) {
  topple($s3_b);
}
  echo rtrim(pileString($s1) . '\nplus\n\n' . pileString($s2) . '\nequals\n\n' . pileString($s3_a)), PHP_EOL;
  echo rtrim('and\n\n' . pileString($s2) . '\nplus\n\n' . pileString($s1) . '\nalso equals\n\n' . pileString($s3_b)), PHP_EOL;
  echo rtrim('Addition of identity sandpile:\n'), PHP_EOL;
  $s3 = [3, 3, 3, 3, 3, 3, 3, 3, 3];
  $s3_id = [2, 1, 2, 1, 0, 1, 2, 1, 2];
  $s4b = plus($s3, $s3_id);
  while (!isStable($s4b)) {
  topple($s4b);
}
  echo rtrim(pileString($s3) . '\nplus\n\n' . pileString($s3_id) . '\nequals\n\n' . pileString($s4b)), PHP_EOL;
  echo rtrim('Addition of identities:\n'), PHP_EOL;
  $s5 = plus($s3_id, $s3_id);
  while (!isStable($s5)) {
  topple($s5);
}
  echo rtrim(pileString($s3_id) . '\nplus\n\n' . pileString($s3_id) . '\nequals\n\n' . pileString($s5)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
