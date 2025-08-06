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
  function digit($ch) {
  $digits = '0123456789';
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0;
};
  function to_int($s) {
  $i = 0;
  $sign = 1;
  if (strlen($s) > 0 && substr($s, 0, 1 - 0) == '-') {
  $sign = -1;
  $i = 1;
}
  $num = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $num = $num * 10 + digit($ch);
  $i = $i + 1;
};
  return $sign * $num;
};
  function split($s, $sep) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function mochi_serialize($node) {
  return (function($__v) {
  if ($__v['__tag'] === "Empty") {
    return 'null';
  } elseif ($__v['__tag'] === "Node") {
    $l = $__v["left"];
    $v = $__v["value"];
    $r = $__v["right"];
    return _str($v) . ',' . mochi_serialize($l) . ',' . mochi_serialize($r);
  }
})($node);
};
  function build($nodes, $idx) {
  $value = $nodes[$idx];
  if ($value == 'null') {
  return ['node' => ['__tag' => 'Empty'], 'next' => $idx + 1];
}
  $left_res = build($nodes, $idx + 1);
  $right_res = build($nodes, $left_res['next']);
  $node = ['__tag' => 'Node', 'left' => $left_res['node'], 'value' => to_int($value), 'right' => $right_res['node']];
  return ['node' => $node, 'next' => $right_res['next']];
};
  function deserialize($data) {
  $nodes = explode(',', $data);
  $res = build($nodes, 0);
  return $res['node'];
};
  function five_tree() {
  $left_child = ['__tag' => 'Node', 'left' => 2, 'value' => ['__tag' => 'Empty'], 'right' => ['__tag' => 'Empty']];
  $right_left = ['__tag' => 'Node', 'left' => 4, 'value' => ['__tag' => 'Empty'], 'right' => ['__tag' => 'Empty']];
  $right_right = ['__tag' => 'Node', 'left' => 5, 'value' => ['__tag' => 'Empty'], 'right' => ['__tag' => 'Empty']];
  $right_child = ['__tag' => 'Node', 'left' => 3, 'value' => $right_left, 'right' => $right_right];
  return ['__tag' => 'Node', 'left' => 1, 'value' => $left_child, 'right' => $right_child];
};
  function main() {
  $root = five_tree();
  $serial = mochi_serialize($root);
  echo rtrim($serial), PHP_EOL;
  $rebuilt = deserialize($serial);
  $serial2 = mochi_serialize($rebuilt);
  echo rtrim($serial2), PHP_EOL;
  echo rtrim(($serial == $serial2 ? 'true' : 'false')), PHP_EOL;
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
