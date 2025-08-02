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
  $EVEN_ROOT = 0;
  $ODD_ROOT = 1;
  function newNode($len) {
  global $EVEN_ROOT, $ODD_ROOT;
  return ['length' => $len, 'edges' => [], 'suffix' => 0];
};
  function eertree($s) {
  global $EVEN_ROOT, $ODD_ROOT;
  $tree = [];
  $tree = array_merge($tree, [['length' => 0, 'suffix' => $ODD_ROOT, 'edges' => []]]);
  $tree = array_merge($tree, [['length' => -1, 'suffix' => $ODD_ROOT, 'edges' => []]]);
  $suffix = $ODD_ROOT;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $n = $suffix;
  $k = 0;
  while (true) {
  $k = intval($tree[$n]['length']);
  $b = $i - $k - 1;
  if ($b >= 0 && substr($s, $b, $b + 1 - $b) == $c) {
  break;
}
  $n = intval($tree[$n]['suffix']);
};
  $edges = $tree[$n]['edges'];
  if (array_key_exists($c, $edges)) {
  $suffix = $edges[$c];
  $i = $i + 1;
  continue;
}
  $suffix = count($tree);
  $tree = array_merge($tree, [newNode($k + 2)]);
  $edges[$c] = $suffix;
  $tree[$n]['edges'] = $edges;
  if ((intval($tree[$suffix]['length'])) == 1) {
  $tree[$suffix]['suffix'] = 0;
  $i = $i + 1;
  continue;
}
  while (true) {
  $n = intval($tree[$n]['suffix']);
  $b = $i - (intval($tree[$n]['length'])) - 1;
  if ($b >= 0 && substr($s, $b, $b + 1 - $b) == $c) {
  break;
}
};
  $en = $tree[$n]['edges'];
  $tree[$suffix]['suffix'] = $en[$c];
  $i = $i + 1;
};
  return $tree;
};
  function child($tree, $idx, $p, $acc) {
  global $EVEN_ROOT, $ODD_ROOT;
  $edges = $tree[$idx]['edges'];
  foreach (array_keys($edges) as $ch) {
  $nxt = $edges[$ch];
  $pal = $ch . $p . $ch;
  $acc = array_merge($acc, [$pal]);
  $acc = child($tree, $nxt, $pal, $acc);
};
  return $acc;
};
  function subPalindromes($tree) {
  global $EVEN_ROOT, $ODD_ROOT;
  $res = [];
  $res = child($tree, $EVEN_ROOT, '', $res);
  $oEdges = $tree[$ODD_ROOT]['edges'];
  foreach (array_keys($oEdges) as $ch) {
  $res = array_merge($res, [$ch]);
  $res = child($tree, $oEdges[$ch], $ch, $res);
};
  return $res;
};
  function main() {
  global $EVEN_ROOT, $ODD_ROOT;
  $tree = eertree('eertree');
  $subs = subPalindromes($tree);
  echo rtrim(_str($subs)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
