<?php
function concatInts(&$a, &$b) {
  global $concatAny, $i, $j, $l, $m;
  $out = [];
  foreach ($a as $v) {
  $out = array_merge($out, [$v]);
};
  foreach ($b as $v) {
  $out = array_merge($out, [$v]);
};
  return $out;
}
function concatAny(&$a, &$b) {
  global $concatInts, $i, $j, $l, $m;
  $out = [];
  foreach ($a as $v) {
  $out = array_merge($out, [$v]);
};
  foreach ($b as $v) {
  $out = array_merge($out, [$v]);
};
  return $out;
}
$a = [1, 2, 3];
$b = [7, 12, 60];
echo json_encode(concatInts($a, $b), 1344), PHP_EOL;
$i = [1, 2, 3];
$j = ["Crosby", "Stills", "Nash", "Young"];
echo json_encode(concatAny($i, $j), 1344), PHP_EOL;
$l = [1, 2, 3];
$m = [7, 12, 60];
echo json_encode(concatInts($l, $m), 1344), PHP_EOL;
