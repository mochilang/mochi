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
$__start_mem = memory_get_usage();
$__start = _now();
  function merge_two_binary_trees($t1, $t2) {
  global $tree1, $tree2, $merged_tree;
  return (function($__v) {
  if ($__v['__tag'] === "Leaf") {
    return $t2;
  } elseif ($__v['__tag'] === "Node") {
    $l1 = $__v["left"];
    $v1 = $__v["value"];
    $r1 = $__v["right"];
    return (function($__v) {
  if ($__v['__tag'] === "Leaf") {
    return $t1;
  } elseif ($__v['__tag'] === "Node") {
    $l2 = $__v["left"];
    $v2 = $__v["value"];
    $r2 = $__v["right"];
    return ['__tag' => 'Node', 'left' => merge_two_binary_trees($l1, $l2), 'value' => $v1 + $v2, 'right' => merge_two_binary_trees($r1, $r2)];
  }
})($t2);
  }
})($t1);
};
  function is_leaf($t) {
  global $tree1, $tree2, $merged_tree;
  return (function($__v) {
  if ($__v['__tag'] === "Leaf") {
    return true;
  } else {
    return false;
  }
})($t);
};
  function get_left($t) {
  global $tree1, $tree2, $merged_tree;
  return (function($__v) {
  if ($__v['__tag'] === "Node") {
    $l = $__v["left"];
    return $l;
  } else {
    return ['__tag' => 'Leaf'];
  }
})($t);
};
  function get_right($t) {
  global $tree1, $tree2, $merged_tree;
  return (function($__v) {
  if ($__v['__tag'] === "Node") {
    $r = $__v["right"];
    return $r;
  } else {
    return ['__tag' => 'Leaf'];
  }
})($t);
};
  function get_value($t) {
  global $tree1, $tree2, $merged_tree;
  return (function($__v) {
  if ($__v['__tag'] === "Node") {
    $v = $__v["value"];
    return $v;
  } else {
    return 0;
  }
})($t);
};
  function print_preorder($t) {
  global $tree1, $tree2, $merged_tree;
  if (!is_leaf($t)) {
  $v = get_value($t);
  $l = get_left($t);
  $r = get_right($t);
  echo rtrim(json_encode($v, 1344)), PHP_EOL;
  print_preorder($l);
  print_preorder($r);
}
};
  $tree1 = ['__tag' => 'Node', 'left' => ['__tag' => 'Node', 'left' => ['__tag' => 'Node', 'left' => ['__tag' => 'Leaf'], 'value' => 4, 'right' => ['__tag' => 'Leaf']], 'value' => 2, 'right' => ['__tag' => 'Leaf']], 'value' => 1, 'right' => ['__tag' => 'Node', 'left' => ['__tag' => 'Leaf'], 'value' => 3, 'right' => ['__tag' => 'Leaf']]];
  $tree2 = ['__tag' => 'Node', 'left' => ['__tag' => 'Node', 'left' => ['__tag' => 'Leaf'], 'value' => 4, 'right' => ['__tag' => 'Node', 'left' => ['__tag' => 'Leaf'], 'value' => 9, 'right' => ['__tag' => 'Leaf']]], 'value' => 2, 'right' => ['__tag' => 'Node', 'left' => ['__tag' => 'Leaf'], 'value' => 6, 'right' => ['__tag' => 'Node', 'left' => ['__tag' => 'Leaf'], 'value' => 5, 'right' => ['__tag' => 'Leaf']]]];
  echo rtrim('Tree1 is:'), PHP_EOL;
  print_preorder($tree1);
  echo rtrim('Tree2 is:'), PHP_EOL;
  print_preorder($tree2);
  $merged_tree = merge_two_binary_trees($tree1, $tree2);
  echo rtrim('Merged Tree is:'), PHP_EOL;
  print_preorder($merged_tree);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
