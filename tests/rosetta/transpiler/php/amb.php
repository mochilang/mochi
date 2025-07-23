<?php
function amb(&$wordsets, &$res, $idx) {
  global $main;
  if ($idx == count($wordsets)) {
  return true;
}
  $prev = "";
  if ($idx > 0) {
  $prev = $res[$idx - 1];
}
  $i = 0;
  while ($i < count($wordsets[$idx])) {
  $w = $wordsets[$idx][$i];
  if ($idx == 0 || substr($prev, strlen($prev) - 1, strlen($prev) - strlen($prev) - 1) == substr($w, 0, 1 - 0)) {
  $res[$idx] = $w;
  if (amb($wordsets, $res, $idx + 1)) {
  return true;
};
}
  $i = $i + 1;
};
  return false;
}
function main() {
  global $amb;
  $wordset = [["the", "that", "a"], ["frog", "elephant", "thing"], ["walked", "treaded", "grows"], ["slowly", "quickly"]];
  $res = [];
  $i = 0;
  while ($i < count($wordset)) {
  $res = array_merge($res, [""]);
  $i = $i + 1;
};
  if (amb($wordset, $res, 0)) {
  $out = "[" . $res[0];
  $j = 1;
  while ($j < count($res)) {
  $out = $out . " " . $res[$j];
  $j = $j + 1;
};
  $out = $out . "]";
  echo $out, PHP_EOL;
} else {
  echo "No amb found", PHP_EOL;
}
}
main();
