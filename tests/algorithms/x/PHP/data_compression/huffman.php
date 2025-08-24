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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_freq($n) {
  return (function($__v) {
  if ($__v['__tag'] === "Leaf") {
    $f = $__v["freq"];
    return $f;
  } elseif ($__v['__tag'] === "Node") {
    $f = $__v["freq"];
    return $f;
  }
})($n);
};
  function sort_nodes($nodes) {
  $arr = $nodes;
  $i = 1;
  while ($i < count($arr)) {
  $key = $arr[$i];
  $j = $i - 1;
  while ($j >= 0 && get_freq($arr[$j]) > get_freq($key)) {
  $arr[$j + 1] = $arr[$j];
  $j = $j - 1;
};
  $arr[$j + 1] = $key;
  $i = $i + 1;
};
  return $arr;
};
  function rest($nodes) {
  $res = [];
  $i = 1;
  while ($i < count($nodes)) {
  $res = _append($res, $nodes[$i]);
  $i = $i + 1;
};
  return $res;
};
  function count_freq($text) {
  $chars = [];
  $freqs = [];
  $i = 0;
  while ($i < strlen($text)) {
  $c = substr($text, $i, $i + 1 - $i);
  $j = 0;
  $found = false;
  while ($j < count($chars)) {
  if ($chars[$j] == $c) {
  $freqs[$j] = $freqs[$j] + 1;
  $found = true;
  break;
}
  $j = $j + 1;
};
  if (!$found) {
  $chars = _append($chars, $c);
  $freqs = _append($freqs, 1);
}
  $i = $i + 1;
};
  $leaves = [];
  $k = 0;
  while ($k < count($chars)) {
  $leaves = _append($leaves, ['__tag' => $Leaf, 'freq' => $freqs[$k], 'symbol' => $chars[$k]]);
  $k = $k + 1;
};
  return sort_nodes($leaves);
};
  function build_tree($nodes) {
  $arr = $nodes;
  while (count($arr) > 1) {
  $left = $arr[0];
  $arr = rest($arr);
  $right = $arr[0];
  $arr = rest($arr);
  $node = ['__tag' => 'Node', 'freq' => get_freq($left) + get_freq($right), 'left' => $left, 'right' => $right];
  $arr = _append($arr, $node);
  $arr = sort_nodes($arr);
};
  return $arr[0];
};
  function concat_pairs($a, $b) {
  $res = $a;
  $i = 0;
  while ($i < count($b)) {
  $res = _append($res, $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function collect_codes($tree, $prefix) {
  return (function($__v) {
  if ($__v['__tag'] === "Leaf") {
    $s = $__v["symbol"];
    return [[$s, $prefix]];
  } elseif ($__v['__tag'] === "Node") {
    $l = $__v["left"];
    $r = $__v["right"];
    return concat_pairs(collect_codes($l, $prefix . '0'), collect_codes($r, $prefix . '1'));
  }
})($tree);
};
  function find_code($pairs, $ch) {
  $i = 0;
  while ($i < count($pairs)) {
  if ($pairs[$i][0] == $ch) {
  return $pairs[$i][1];
}
  $i = $i + 1;
};
  return '';
};
  function huffman_encode($text) {
  if ($text == '') {
  return '';
}
  $leaves = count_freq($text);
  $tree = build_tree($leaves);
  $codes = collect_codes($tree, '');
  $encoded = '';
  $i = 0;
  while ($i < strlen($text)) {
  $c = substr($text, $i, $i + 1 - $i);
  $encoded = $encoded . find_code($codes, $c) . ' ';
  $i = $i + 1;
};
  return $encoded;
};
  echo rtrim(huffman_encode('beep boop beer!')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
