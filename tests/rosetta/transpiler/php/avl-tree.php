<?php
ini_set('memory_limit', '-1');
function Node($data) {
  global $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  return ['Data' => $data, 'Balance' => 0, 'Link' => [null, null]];
}
function getLink($n, $dir) {
  global $Node, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  return ($n['Link'])[$dir];
}
function setLink($n, $dir, $v) {
  global $Node, $getLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  $links = $n['Link'];
  $links[$dir] = $v;
  $n['Link'] = $links;
}
function opp($dir) {
  global $Node, $getLink, $setLink, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  return 1 - $dir;
}
function single($root, $dir) {
  global $Node, $getLink, $setLink, $opp, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  $tmp = getLink($root, opp($dir));
  setLink($root, opp($dir), getLink($tmp, $dir));
  setLink($tmp, $dir, $root);
  return $tmp;
}
function double($root, $dir) {
  global $Node, $getLink, $setLink, $opp, $single, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  $tmp = getLink(getLink($root, opp($dir)), $dir);
  setLink(getLink($root, opp($dir)), $dir, getLink($tmp, opp($dir)));
  setLink($tmp, opp($dir), getLink($root, opp($dir)));
  setLink($root, opp($dir), $tmp);
  $tmp = getLink($root, opp($dir));
  setLink($root, opp($dir), getLink($tmp, $dir));
  setLink($tmp, $dir, $root);
  return $tmp;
}
function adjustBalance($root, $dir, $bal) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  $n = getLink($root, $dir);
  $nn = getLink($n, opp($dir));
  if ($nn['Balance'] == 0) {
  $root['Balance'] = 0;
  $n['Balance'] = 0;
} else {
  if ($nn['Balance'] == $bal) {
  $root['Balance'] = -$bal;
  $n['Balance'] = 0;
} else {
  $root['Balance'] = 0;
  $n['Balance'] = $bal;
};
}
  $nn['Balance'] = 0;
}
function insertBalance($root, $dir) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  $n = getLink($root, $dir);
  $bal = 2 * $dir - 1;
  if ($n['Balance'] == $bal) {
  $root['Balance'] = 0;
  $n['Balance'] = 0;
  return single($root, opp($dir));
}
  adjustBalance($root, $dir, $bal);
  return double($root, opp($dir));
}
function insertR($root, $data) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  if ($root == null) {
  return ['node' => Node($data), 'done' => false];
}
  $node = $root;
  $dir = 0;
  if ((ord($node['Data'])) < $data) {
  $dir = 1;
}
  $r = insertR(getLink($node, $dir), $data);
  setLink($node, $dir, $r['node']);
  if ($r['done']) {
  return ['node' => $node, 'done' => true];
}
  $node['Balance'] = (ord($node['Balance'])) + (2 * $dir - 1);
  if ($node['Balance'] == 0) {
  return ['node' => $node, 'done' => true];
}
  if ($node['Balance'] == 1 || $node['Balance'] == (-1)) {
  return ['node' => $node, 'done' => false];
}
  return ['node' => insertBalance($node, $dir), 'done' => true];
}
function Insert($tree, $data) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  $r = insertR($tree, $data);
  return $r['node'];
}
function removeBalance($root, $dir) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeR, $Remove, $indentStr, $dumpNode, $dump, $main;
  $n = getLink($root, opp($dir));
  $bal = 2 * $dir - 1;
  if ($n['Balance'] == (-$bal)) {
  $root['Balance'] = 0;
  $n['Balance'] = 0;
  return ['node' => single($root, $dir), 'done' => false];
}
  if ($n['Balance'] == $bal) {
  adjustBalance($root, opp($dir), (-$bal));
  return ['node' => double($root, $dir), 'done' => false];
}
  $root['Balance'] = -$bal;
  $n['Balance'] = $bal;
  return ['node' => single($root, $dir), 'done' => true];
}
function removeR($root, $data) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $Remove, $indentStr, $dumpNode, $dump, $main;
  if ($root == null) {
  return ['node' => null, 'done' => false];
}
  $node = $root;
  if ((ord($node['Data'])) == $data) {
  if (getLink($node, 0) == null) {
  return ['node' => getLink($node, 1), 'done' => false];
};
  if (getLink($node, 1) == null) {
  return ['node' => getLink($node, 0), 'done' => false];
};
  $heir = getLink($node, 0);
  while (getLink($heir, 1) != null) {
  $heir = getLink($heir, 1);
};
  $node['Data'] = $heir['Data'];
  $data = ord($heir['Data']);
}
  $dir = 0;
  if ((ord($node['Data'])) < $data) {
  $dir = 1;
}
  $r = removeR(getLink($node, $dir), $data);
  setLink($node, $dir, $r['node']);
  if ($r['done']) {
  return ['node' => $node, 'done' => true];
}
  $node['Balance'] = (ord($node['Balance'])) + 1 - 2 * $dir;
  if ($node['Balance'] == 1 || $node['Balance'] == (-1)) {
  return ['node' => $node, 'done' => true];
}
  if ($node['Balance'] == 0) {
  return ['node' => $node, 'done' => false];
}
  return removeBalance($node, $dir);
}
function Remove($tree, $data) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $indentStr, $dumpNode, $dump, $main;
  $r = removeR($tree, $data);
  return $r['node'];
}
function indentStr($n) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $dumpNode, $dump, $main;
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . ' ';
  $i = $i + 1;
};
  return $s;
}
function dumpNode($node, $indent, $comma) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dump, $main;
  $sp = indentStr($indent);
  if ($node == null) {
  $line = $sp . 'null';
  if ($comma) {
  $line = $line . ',';
};
  echo rtrim($line), PHP_EOL;
} else {
  echo rtrim($sp . '{'), PHP_EOL;
  echo rtrim(indentStr($indent + 3) . '"Data": ' . json_encode($node['Data'], 1344) . ','), PHP_EOL;
  echo rtrim(indentStr($indent + 3) . '"Balance": ' . json_encode($node['Balance'], 1344) . ','), PHP_EOL;
  echo rtrim(indentStr($indent + 3) . '"Link": ['), PHP_EOL;
  dumpNode(getLink($node, 0), $indent + 6, true);
  dumpNode(getLink($node, 1), $indent + 6, false);
  echo rtrim(indentStr($indent + 3) . ']'), PHP_EOL;
  $end = $sp . '}';
  if ($comma) {
  $end = $end . ',';
};
  echo rtrim($end), PHP_EOL;
}
}
function dump($node, $indent) {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $main;
  dumpNode($node, $indent, false);
}
function main() {
  global $Node, $getLink, $setLink, $opp, $single, $double, $adjustBalance, $insertBalance, $insertR, $Insert, $removeBalance, $removeR, $Remove, $indentStr, $dumpNode, $dump;
  $tree = null;
  echo rtrim('Empty tree:'), PHP_EOL;
  dump($tree, 0);
  echo rtrim(''), PHP_EOL;
  echo rtrim('Insert test:'), PHP_EOL;
  $tree = Insert($tree, 3);
  $tree = Insert($tree, 1);
  $tree = Insert($tree, 4);
  $tree = Insert($tree, 1);
  $tree = Insert($tree, 5);
  dump($tree, 0);
  echo rtrim(''), PHP_EOL;
  echo rtrim('Remove test:'), PHP_EOL;
  $tree = Remove($tree, 3);
  $tree = Remove($tree, 1);
  $t = $tree;
  $t['Balance'] = 0;
  $tree = $t;
  dump($tree, 0);
}
main();
