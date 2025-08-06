<?php
ini_set('memory_limit', '-1');
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
$LABEL = 0;
$COLOR = 1;
$PARENT = 2;
$LEFT = 3;
$RIGHT = 4;
$NEG_ONE = -1;
function make_tree() {
  global $LABEL, $COLOR, $PARENT, $LEFT, $RIGHT, $NEG_ONE;
  return ['nodes' => [], 'root' => -1];
}
function rotate_left(&$t, $x) {
  global $LABEL, $COLOR, $PARENT, $LEFT, $RIGHT, $NEG_ONE;
  $nodes = $t['nodes'];
  $y = $nodes[$x][$RIGHT];
  $yLeft = $nodes[$y][$LEFT];
  $nodes[$x][$RIGHT] = $yLeft;
  if ($yLeft != $NEG_ONE) {
  $nodes[$yLeft][$PARENT] = $x;
}
  $xParent = $nodes[$x][$PARENT];
  $nodes[$y][$PARENT] = $xParent;
  if ($xParent == $NEG_ONE) {
  $t['root'] = $y;
} else {
  if ($x == $nodes[$xParent][$LEFT]) {
  $nodes[$xParent][$LEFT] = $y;
} else {
  $nodes[$xParent][$RIGHT] = $y;
};
}
  $nodes[$y][$LEFT] = $x;
  $nodes[$x][$PARENT] = $y;
  $t['nodes'] = $nodes;
  return $t;
}
function rotate_right(&$t, $x) {
  global $LABEL, $COLOR, $PARENT, $LEFT, $RIGHT, $NEG_ONE;
  $nodes = $t['nodes'];
  $y = $nodes[$x][$LEFT];
  $yRight = $nodes[$y][$RIGHT];
  $nodes[$x][$LEFT] = $yRight;
  if ($yRight != $NEG_ONE) {
  $nodes[$yRight][$PARENT] = $x;
}
  $xParent = $nodes[$x][$PARENT];
  $nodes[$y][$PARENT] = $xParent;
  if ($xParent == $NEG_ONE) {
  $t['root'] = $y;
} else {
  if ($x == $nodes[$xParent][$RIGHT]) {
  $nodes[$xParent][$RIGHT] = $y;
} else {
  $nodes[$xParent][$LEFT] = $y;
};
}
  $nodes[$y][$RIGHT] = $x;
  $nodes[$x][$PARENT] = $y;
  $t['nodes'] = $nodes;
  return $t;
}
function insert_fix(&$t, $z) {
  global $LABEL, $COLOR, $PARENT, $LEFT, $RIGHT, $NEG_ONE;
  $nodes = $t['nodes'];
  while ($z != $t['root'] && $nodes[$nodes[$z][$PARENT]][$COLOR] == 1) {
  if ($nodes[$z][$PARENT] == $nodes[$nodes[$nodes[$z][$PARENT]][$PARENT]][$LEFT]) {
  $y = $nodes[$nodes[$nodes[$z][$PARENT]][$PARENT]][$RIGHT];
  if ($y != $NEG_ONE && $nodes[$y][$COLOR] == 1) {
  $nodes[$nodes[$z][$PARENT]][$COLOR] = 0;
  $nodes[$y][$COLOR] = 0;
  $gp = $nodes[$nodes[$z][$PARENT]][$PARENT];
  $nodes[$gp][$COLOR] = 1;
  $z = $gp;
} else {
  if ($z == $nodes[$nodes[$z][$PARENT]][$RIGHT]) {
  $z = $nodes[$z][$PARENT];
  $t['nodes'] = $nodes;
  $t = rotate_left($t, $z);
  $nodes = $t['nodes'];
};
  $nodes[$nodes[$z][$PARENT]][$COLOR] = 0;
  $gp = $nodes[$nodes[$z][$PARENT]][$PARENT];
  $nodes[$gp][$COLOR] = 1;
  $t['nodes'] = $nodes;
  $t = rotate_right($t, $gp);
  $nodes = $t['nodes'];
};
} else {
  $y = $nodes[$nodes[$nodes[$z][$PARENT]][$PARENT]][$LEFT];
  if ($y != $NEG_ONE && $nodes[$y][$COLOR] == 1) {
  $nodes[$nodes[$z][$PARENT]][$COLOR] = 0;
  $nodes[$y][$COLOR] = 0;
  $gp = $nodes[$nodes[$z][$PARENT]][$PARENT];
  $nodes[$gp][$COLOR] = 1;
  $z = $gp;
} else {
  if ($z == $nodes[$nodes[$z][$PARENT]][$LEFT]) {
  $z = $nodes[$z][$PARENT];
  $t['nodes'] = $nodes;
  $t = rotate_right($t, $z);
  $nodes = $t['nodes'];
};
  $nodes[$nodes[$z][$PARENT]][$COLOR] = 0;
  $gp = $nodes[$nodes[$z][$PARENT]][$PARENT];
  $nodes[$gp][$COLOR] = 1;
  $t['nodes'] = $nodes;
  $t = rotate_left($t, $gp);
  $nodes = $t['nodes'];
};
}
};
  $nodes = $t['nodes'];
  $nodes[$t['root']][$COLOR] = 0;
  $t['nodes'] = $nodes;
  return $t;
}
function tree_insert(&$t, $v) {
  global $LABEL, $COLOR, $PARENT, $LEFT, $RIGHT, $NEG_ONE;
  $nodes = $t['nodes'];
  $node = [$v, 1, -1, -1, -1];
  $nodes = _append($nodes, $node);
  $idx = count($nodes) - 1;
  $y = $NEG_ONE;
  $x = $t['root'];
  while ($x != $NEG_ONE) {
  $y = $x;
  if ($v < $nodes[$x][$LABEL]) {
  $x = $nodes[$x][$LEFT];
} else {
  $x = $nodes[$x][$RIGHT];
}
};
  $nodes[$idx][$PARENT] = $y;
  if ($y == $NEG_ONE) {
  $t['root'] = $idx;
} else {
  if ($v < $nodes[$y][$LABEL]) {
  $nodes[$y][$LEFT] = $idx;
} else {
  $nodes[$y][$RIGHT] = $idx;
};
}
  $t['nodes'] = $nodes;
  $t = insert_fix($t, $idx);
  return $t;
}
function inorder($t, $x, $acc) {
  global $LABEL, $COLOR, $PARENT, $LEFT, $RIGHT, $NEG_ONE;
  if ($x == $NEG_ONE) {
  return $acc;
}
  $acc = inorder($t, $t['nodes'][$x][$LEFT], $acc);
  $acc = _append($acc, $t['nodes'][$x][$LABEL]);
  $acc = inorder($t, $t['nodes'][$x][$RIGHT], $acc);
  return $acc;
}
function main() {
  global $LABEL, $COLOR, $PARENT, $LEFT, $RIGHT, $NEG_ONE;
  $t = make_tree();
  $values = [10, 20, 30, 15, 25, 5, 1];
  $i = 0;
  while ($i < count($values)) {
  $t = tree_insert($t, $values[$i]);
  $i = $i + 1;
};
  $res = [];
  $res = inorder($t, $t['root'], $res);
  echo rtrim(_str($res)), PHP_EOL;
}
main();
