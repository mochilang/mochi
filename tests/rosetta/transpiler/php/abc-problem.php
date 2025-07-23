<?php
function fields($s) {
  global $canSpell, $newSpeller, $main;
  $res = [];
  $cur = "";
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c == " ") {
  if (strlen($cur) > 0) {
  $res = array_merge($res, [$cur]);
  $cur = "";
};
} else {
  $cur = $cur . $c;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $res = array_merge($res, [$cur]);
}
  return $res;
}
function canSpell($word, $blks) {
  global $fields, $newSpeller, $main;
  if (strlen($word) == 0) {
  return true;
}
  $c = strtolower(substr($word, 0, 1 - 0));
  $i = 0;
  while ($i < count($blks)) {
  $b = $blks[$i];
  if ($c == strtolower(substr($b, 0, 1 - 0)) || $c == strtolower(substr($b, 1, 2 - 1))) {
  $rest = [];
  $j = 0;
  while ($j < count($blks)) {
  if ($j != $i) {
  $rest = array_merge($rest, [$blks[$j]]);
}
  $j = $j + 1;
};
  if (canSpell(substr($word, 1), $rest)) {
  return true;
};
}
  $i = $i + 1;
};
  return false;
}
function newSpeller($blocks) {
  global $fields, $canSpell, $main;
  $bl = fields($blocks);
  return function($w) use ($blocks, $bl) {
  return canSpell($w, $bl);
};
}
function main() {
  global $fields, $canSpell, $newSpeller;
  $sp = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM");
  foreach (["A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"] as $word) {
  echo $word . " " . json_encode($sp($word), 1344), PHP_EOL;
};
}
main();
