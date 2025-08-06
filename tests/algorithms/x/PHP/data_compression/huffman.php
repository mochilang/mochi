<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
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
}
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
}
function rest($nodes) {
  $res = [];
  $i = 1;
  while ($i < count($nodes)) {
  $res = _append($res, $nodes[$i]);
  $i = $i + 1;
};
  return $res;
}
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
  $leaves = _append($leaves, ['__tag' => $Leaf, 'symbol' => $chars[$k], 'freq' => $freqs[$k]]);
  $k = $k + 1;
};
  return sort_nodes($leaves);
}
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
}
function concat_pairs($a, $b) {
  $res = $a;
  $i = 0;
  while ($i < count($b)) {
  $res = _append($res, $b[$i]);
  $i = $i + 1;
};
  return $res;
}
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
}
function find_code($pairs, $ch) {
  $i = 0;
  while ($i < count($pairs)) {
  if ($pairs[$i][0] == $ch) {
  return $pairs[$i][1];
}
  $i = $i + 1;
};
  return '';
}
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
}
echo rtrim(huffman_encode('beep boop beer!')), PHP_EOL;
