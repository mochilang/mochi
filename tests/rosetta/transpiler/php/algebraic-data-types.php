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
  function node($cl, $le, $aa, $ri) {
  global $tr, $i;
  return ['cl' => $cl, 'le' => $le, 'aa' => $aa, 'ri' => $ri];
};
  function treeString($t) {
  global $tr, $i;
  if ($t == null) {
  return 'E';
}
  $m = $t;
  return 'T(' . $m['cl'] . ', ' . treeString($m['le']) . ', ' . _str($m['aa']) . ', ' . treeString($m['ri']) . ')';
};
  function balance($t) {
  global $tr, $i;
  if ($t == null) {
  return $t;
}
  $m = $t;
  if ($m['cl'] != 'B') {
  return $t;
}
  $le = $m['le'];
  $ri = $m['ri'];
  if ($le != null) {
  $leMap = $le;
  if ($leMap['cl'] == 'R') {
  $lele = $leMap['le'];
  if ($lele != null) {
  $leleMap = $lele;
  if ($leleMap['cl'] == 'R') {
  return node('R', node('B', $leleMap['le'], $leleMap['aa'], $leleMap['ri']), $leMap['aa'], node('B', $leMap['ri'], $m['aa'], $ri));
};
};
  $leri = $leMap['ri'];
  if ($leri != null) {
  $leriMap = $leri;
  if ($leriMap['cl'] == 'R') {
  return node('R', node('B', $leMap['le'], $leMap['aa'], $leriMap['le']), $leriMap['aa'], node('B', $leriMap['ri'], $m['aa'], $ri));
};
};
};
}
  if ($ri != null) {
  $riMap = $ri;
  if ($riMap['cl'] == 'R') {
  $rile = $riMap['le'];
  if ($rile != null) {
  $rileMap = $rile;
  if ($rileMap['cl'] == 'R') {
  return node('R', node('B', $m['le'], $m['aa'], $rileMap['le']), $rileMap['aa'], node('B', $rileMap['ri'], $riMap['aa'], $riMap['ri']));
};
};
  $riri = $riMap['ri'];
  if ($riri != null) {
  $ririMap = $riri;
  if ($ririMap['cl'] == 'R') {
  return node('R', node('B', $m['le'], $m['aa'], $riMap['le']), $riMap['aa'], node('B', $ririMap['le'], $ririMap['aa'], $ririMap['ri']));
};
};
};
}
  return $t;
};
  function ins($tr, $x) {
  global $i;
  if ($tr == null) {
  return node('R', null, $x, null);
}
  if ($x < $tr['aa']) {
  return balance(node($tr['cl'], ins($tr['le'], $x), $tr['aa'], $tr['ri']));
}
  if ($x > $tr['aa']) {
  return balance(node($tr['cl'], $tr['le'], $tr['aa'], ins($tr['ri'], $x)));
}
  return $tr;
};
  function insert($tr, $x) {
  global $i;
  $t = ins($tr, $x);
  if ($t == null) {
  return null;
}
  $m = $t;
  return node('B', $m['le'], $m['aa'], $m['ri']);
};
  $tr = null;
  $i = 1;
  while ($i <= 16) {
  $tr = insert($tr, $i);
  $i = $i + 1;
}
  echo rtrim(treeString($tr)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
