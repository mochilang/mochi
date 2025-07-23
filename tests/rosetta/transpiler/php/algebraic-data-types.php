<?php
function node($cl, $le, $aa, $ri) {
  global $treeString, $balance, $ins, $insert, $tr, $i;
  return ["cl" => $cl, "le" => $le, "aa" => $aa, "ri" => $ri];
}
function treeString($t) {
  global $node, $balance, $ins, $insert, $tr, $i;
  if ($t == null) {
  return "E";
}
  $m = $t;
  return "T(" . $m["cl"] . ", " . treeString($m["le"]) . ", " . json_encode($m["aa"], 1344) . ", " . treeString($m["ri"]) . ")";
}
function balance($t) {
  global $node, $treeString, $ins, $insert, $tr, $i;
  if ($t == null) {
  return $t;
}
  $m = $t;
  if ($m["cl"] != "B") {
  return $t;
}
  $le = $m["le"];
  $ri = $m["ri"];
  if ($le != null) {
  $leMap = $le;
  if ($leMap["cl"] == "R") {
  $lele = $leMap["le"];
  if ($lele != null) {
  $leleMap = $lele;
  if ($leleMap["cl"] == "R") {
  return node("R", node("B", $leleMap["le"], $leleMap["aa"], $leleMap["ri"]), $leMap["aa"], node("B", $leMap["ri"], $m["aa"], $ri));
};
};
  $leri = $leMap["ri"];
  if ($leri != null) {
  $leriMap = $leri;
  if ($leriMap["cl"] == "R") {
  return node("R", node("B", $leMap["le"], $leMap["aa"], $leriMap["le"]), $leriMap["aa"], node("B", $leriMap["ri"], $m["aa"], $ri));
};
};
};
}
  if ($ri != null) {
  $riMap = $ri;
  if ($riMap["cl"] == "R") {
  $rile = $riMap["le"];
  if ($rile != null) {
  $rileMap = $rile;
  if ($rileMap["cl"] == "R") {
  return node("R", node("B", $m["le"], $m["aa"], $rileMap["le"]), $rileMap["aa"], node("B", $rileMap["ri"], $riMap["aa"], $riMap["ri"]));
};
};
  $riri = $riMap["ri"];
  if ($riri != null) {
  $ririMap = $riri;
  if ($ririMap["cl"] == "R") {
  return node("R", node("B", $m["le"], $m["aa"], $riMap["le"]), $riMap["aa"], node("B", $ririMap["le"], $ririMap["aa"], $ririMap["ri"]));
};
};
};
}
  return $t;
}
function ins($tr, $x) {
  global $node, $treeString, $balance, $insert, $i;
  if ($tr == null) {
  return node("R", null, $x, null);
}
  if ($x < $tr["aa"]) {
  return balance(node($tr["cl"], ins($tr["le"], $x), $tr["aa"], $tr["ri"]));
}
  if ($x > $tr["aa"]) {
  return balance(node($tr["cl"], $tr["le"], $tr["aa"], ins($tr["ri"], $x)));
}
  return $tr;
}
function insert($tr, $x) {
  global $node, $treeString, $balance, $ins, $i;
  $t = ins($tr, $x);
  if ($t == null) {
  return null;
}
  $m = $t;
  return node("B", $m["le"], $m["aa"], $m["ri"]);
}
$tr = null;
$i = 1;
while ($i <= 16) {
  $tr = insert($tr, $i);
  $i = $i + 1;
}
echo treeString($tr), PHP_EOL;
