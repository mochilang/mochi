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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function uf_make($n) {
  $p = [];
  $r = [];
  $i = 0;
  while ($i < $n) {
  $p = _append($p, $i);
  $r = _append($r, 0);
  $i = $i + 1;
};
  return ['parent' => $p, 'rank' => $r];
};
  function uf_find($uf, $x) {
  $p = $uf['parent'];
  if ($p[$x] != $x) {
  $res = uf_find(['parent' => $p, 'rank' => $uf['rank']], $p[$x]);
  $p = $res['uf']['parent'];
  $p[$x] = $res['root'];
  return ['root' => $res['root'], 'uf' => ['parent' => $p, 'rank' => $res['uf']['rank']]];
}
  return ['root' => $x, 'uf' => $uf];
};
  function uf_union($uf, $x, $y) {
  $fr1 = uf_find($uf, $x);
  $uf1 = $fr1['uf'];
  $root1 = $fr1['root'];
  $fr2 = uf_find($uf1, $y);
  $uf1 = $fr2['uf'];
  $root2 = $fr2['root'];
  if ($root1 == $root2) {
  return $uf1;
}
  $p = $uf1['parent'];
  $r = $uf1['rank'];
  if ($r[$root1] > $r[$root2]) {
  $p[$root2] = $root1;
} else {
  if ($r[$root1] < $r[$root2]) {
  $p[$root1] = $root2;
} else {
  $p[$root2] = $root1;
  $r[$root1] = $r[$root1] + 1;
};
}
  return ['parent' => $p, 'rank' => $r];
};
  function boruvka($n, $edges) {
  $uf = uf_make($n);
  $num_components = $n;
  $mst = [];
  while ($num_components > 1) {
  $cheap = [];
  $i = 0;
  while ($i < $n) {
  $cheap = _append($cheap, 0 - 1);
  $i = $i + 1;
};
  $idx = 0;
  while ($idx < count($edges)) {
  $e = $edges[$idx];
  $fr1 = uf_find($uf, $e['u']);
  $uf = $fr1['uf'];
  $set1 = $fr1['root'];
  $fr2 = uf_find($uf, $e['v']);
  $uf = $fr2['uf'];
  $set2 = $fr2['root'];
  if ($set1 != $set2) {
  if ($cheap[$set1] == 0 - 1 || $edges[$cheap[$set1]]['w'] > $e['w']) {
  $cheap[$set1] = $idx;
};
  if ($cheap[$set2] == 0 - 1 || $edges[$cheap[$set2]]['w'] > $e['w']) {
  $cheap[$set2] = $idx;
};
}
  $idx = $idx + 1;
};
  $v = 0;
  while ($v < $n) {
  $idxe = $cheap[$v];
  if ($idxe != 0 - 1) {
  $e = $edges[$idxe];
  $fr1 = uf_find($uf, $e['u']);
  $uf = $fr1['uf'];
  $set1 = $fr1['root'];
  $fr2 = uf_find($uf, $e['v']);
  $uf = $fr2['uf'];
  $set2 = $fr2['root'];
  if ($set1 != $set2) {
  $mst = _append($mst, $e);
  $uf = uf_union($uf, $set1, $set2);
  $num_components = $num_components - 1;
};
}
  $v = $v + 1;
};
};
  return $mst;
};
  function main() {
  $edges = [['u' => 0, 'v' => 1, 'w' => 1], ['u' => 0, 'v' => 2, 'w' => 2], ['u' => 2, 'v' => 3, 'w' => 3]];
  $mst = boruvka(4, $edges);
  foreach ($mst as $e) {
  echo rtrim(_str($e['u']) . ' - ' . _str($e['v']) . ' : ' . _str($e['w'])), PHP_EOL;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
