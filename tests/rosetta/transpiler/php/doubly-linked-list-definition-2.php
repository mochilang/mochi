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
  function newList() {
  return ['nodes' => [], 'head' => 0, 'tail' => 0, 'nextID' => 1];
};
  function newNode(&$l, $v) {
  $id = intval($l['nextID']);
  $l['nextID'] = $id + 1;
  $nodes = $l['nodes'];
  $n = ['id' => $id, 'value' => $v, 'next' => 0, 'prev' => 0];
  $nodes[$id] = $n;
  $l['nodes'] = $nodes;
  return $n;
};
  function pushFront(&$l, $v) {
  $n = newNode($l, $v);
  $n['next'] = $l['head'];
  if ((intval($l['head'])) != 0) {
  $nodes = $l['nodes'];
  $h = $nodes[intval($l['head'])];
  $h['prev'] = $n['id'];
  $nodes[intval($h['id'])] = $h;
  $l['nodes'] = $nodes;
} else {
  $l['tail'] = $n['id'];
}
  $l['head'] = $n['id'];
  $nodes2 = $l['nodes'];
  $nodes2[intval($n['id'])] = $n;
  $l['nodes'] = $nodes2;
  return $n;
};
  function pushBack(&$l, $v) {
  $n = newNode($l, $v);
  $n['prev'] = $l['tail'];
  if ((intval($l['tail'])) != 0) {
  $nodes = $l['nodes'];
  $t = $nodes[intval($l['tail'])];
  $t['next'] = $n['id'];
  $nodes[intval($t['id'])] = $t;
  $l['nodes'] = $nodes;
} else {
  $l['head'] = $n['id'];
}
  $l['tail'] = $n['id'];
  $nodes2 = $l['nodes'];
  $nodes2[intval($n['id'])] = $n;
  $l['nodes'] = $nodes2;
  return $n;
};
  function insertBefore(&$l, $refID, $v) {
  if ($refID == 0) {
  return pushFront($l, $v);
}
  $nodes = $l['nodes'];
  $ref = $nodes[$refID];
  $n = newNode($l, $v);
  $n['prev'] = $ref['prev'];
  $n['next'] = $ref['id'];
  if ((intval($ref['prev'])) != 0) {
  $p = $nodes[intval($ref['prev'])];
  $p['next'] = $n['id'];
  $nodes[intval($p['id'])] = $p;
} else {
  $l['head'] = $n['id'];
}
  $ref['prev'] = $n['id'];
  $nodes[$refID] = $ref;
  $nodes[intval($n['id'])] = $n;
  $l['nodes'] = $nodes;
  return $n;
};
  function insertAfter(&$l, $refID, $v) {
  if ($refID == 0) {
  return pushBack($l, $v);
}
  $nodes = $l['nodes'];
  $ref = $nodes[$refID];
  $n = newNode($l, $v);
  $n['next'] = $ref['next'];
  $n['prev'] = $ref['id'];
  if ((intval($ref['next'])) != 0) {
  $nx = $nodes[intval($ref['next'])];
  $nx['prev'] = $n['id'];
  $nodes[intval($nx['id'])] = $nx;
} else {
  $l['tail'] = $n['id'];
}
  $ref['next'] = $n['id'];
  $nodes[$refID] = $ref;
  $nodes[intval($n['id'])] = $n;
  $l['nodes'] = $nodes;
  return $n;
};
  function main() {
  $l = newList();
  $e4 = pushBack($l, 4);
  $e1 = pushFront($l, 1);
  insertBefore($l, intval($e4['id']), 3);
  insertAfter($l, intval($e1['id']), 'two');
  $id = intval($l['head']);
  $nodes = $l['nodes'];
  while ($id != 0) {
  $node = $nodes[$id];
  echo rtrim(_str($node['value'])), PHP_EOL;
  $id = intval($node['next']);
};
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
