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
  function split($s, $sep) {
  $out = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if ($i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $out = array_merge($out, [$cur]);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $out = array_merge($out, [$cur]);
  return $out;
};
  function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function trimLeftSpaces($s) {
  $i = 0;
  while ($i < strlen($s) && substr($s, $i, $i + 1 - $i) == ' ') {
  $i = $i + 1;
};
  return substr($s, $i, strlen($s) - $i);
};
  function makeIndent($outline, $tab) {
  $lines = explode('
', $outline);
  $nodes = [];
  foreach ($lines as $line) {
  $line2 = trimLeftSpaces($line);
  $level = (strlen($line) - strlen($line2)) / $tab;
  $nodes = array_merge($nodes, [['level' => $level, 'name' => $line2]]);
};
  return $nodes;
};
  function toNest($nodes, $start, $level, &$n) {
  if ($level == 0) {
  $n['name'] = $nodes[0]['name'];
}
  $i = $start + 1;
  while ($i < count($nodes)) {
  $node = $nodes[$i];
  $lev = intval($node['level']);
  if ($lev == $level + 1) {
  $child = ['name' => $node['name'], 'children' => []];
  toNest($nodes, $i, $level + 1, $child);
  $cs = $n['children'];
  $cs = array_merge($cs, [$child]);
  $n['children'] = $cs;
} else {
  if ($lev <= $level) {
  return;
};
}
  $i = $i + 1;
};
};
  function countLeaves($n) {
  $kids = $n['children'];
  if (count($kids) == 0) {
  return 1;
}
  $total = 0;
  foreach ($kids as $k) {
  $total = $total + countLeaves($k);
};
  return $total;
};
  function nodesByDepth($root, $depth) {
  $levels = [];
  $current = [$root];
  $d = 0;
  while ($d < $depth) {
  $levels = array_merge($levels, [$current]);
  $next = [];
  foreach ($current as $n) {
  $kids = $n['children'];
  foreach ($kids as $k) {
  $next = array_merge($next, [$k]);
};
};
  $current = $next;
  $d = $d + 1;
};
  return $levels;
};
  function toMarkup($n, $cols, $depth) {
  $lines = [];
  $lines = array_merge($lines, ['{| class="wikitable" style="text-align: center;"']);
  $l1 = '|-';
  $lines = array_merge($lines, [$l1]);
  $span = countLeaves($n);
  $lines = array_merge($lines, ['| style="background: ' . $cols[0] . ' " colSpan=' . _str($span) . ' | ' . (strval($n['name']))]);
  $lines = array_merge($lines, [$l1]);
  $lvls = nodesByDepth($n, $depth);
  $lvl = 1;
  while ($lvl < $depth) {
  $nodes = $lvls[$lvl];
  if (count($nodes) == 0) {
  $lines = array_merge($lines, ['|  |']);
} else {
  $idx = 0;
  while ($idx < count($nodes)) {
  $node = $nodes[$idx];
  $span = countLeaves($node);
  $col = $lvl;
  if ($lvl == 1) {
  $col = $idx + 1;
}
  if ($col >= count($cols)) {
  $col = count($cols) - 1;
}
  $cell = '| style="background: ' . $cols[$col] . ' " colspan=' . _str($span) . ' | ' . (strval($node['name']));
  $lines = array_merge($lines, [$cell]);
  $idx = $idx + 1;
};
}
  if ($lvl < $depth - 1) {
  $lines = array_merge($lines, [$l1]);
}
  $lvl = $lvl + 1;
};
  $lines = array_merge($lines, ['|}']);
  return mochi_join($lines, '
');
};
  function main() {
  $outline = 'Display an outline as a nested table.
' . '    Parse the outline to a tree,
' . '        measuring the indent of each line,
' . '        translating the indentation to a nested structure,
' . '        and padding the tree to even depth.
' . '    count the leaves descending from each node,
' . '        defining the width of a leaf as 1,
' . '        and the width of a parent node as a sum.
' . '            (The sum of the widths of its children)
' . '    and write out a table with \'colspan\' values
' . '        either as a wiki table,
' . '        or as HTML.';
  $yellow = '#ffffe6;';
  $orange = '#ffebd2;';
  $green = '#f0fff0;';
  $blue = '#e6ffff;';
  $pink = '#ffeeff;';
  $cols = [$yellow, $orange, $green, $blue, $pink];
  $nodes = makeIndent($outline, 4);
  $n = ['name' => '', 'children' => []];
  toNest($nodes, 0, 0, $n);
  echo rtrim(toMarkup($n, $cols, 4)), PHP_EOL;
  echo rtrim('
'), PHP_EOL;
  $outline2 = 'Display an outline as a nested table.
' . '    Parse the outline to a tree,
' . '        measuring the indent of each line,
' . '        translating the indentation to a nested structure,
' . '        and padding the tree to even depth.
' . '    count the leaves descending from each node,
' . '        defining the width of a leaf as 1,
' . '        and the width of a parent node as a sum.
' . '            (The sum of the widths of its children)
' . '            Propagating the sums upward as necessary.
' . '    and write out a table with \'colspan\' values
' . '        either as a wiki table,
' . '        or as HTML.
' . '    Optionally add color to the nodes.';
  $cols2 = [$blue, $yellow, $orange, $green, $pink];
  $nodes2 = makeIndent($outline2, 4);
  $n2 = ['name' => '', 'children' => []];
  toNest($nodes2, 0, 0, $n2);
  echo rtrim(toMarkup($n2, $cols2, 4)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
