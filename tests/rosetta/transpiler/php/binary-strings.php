<?php
ini_set('memory_limit', '-1');
function char($n) {
  global $fromBytes, $b, $c, $d, $i, $z, $sub, $f, $val, $rem;
  $letters = 'abcdefghijklmnopqrstuvwxyz';
  $idx = $n - 97;
  if ($idx < 0 || $idx >= strlen($letters)) {
  return '?';
}
  return substr($letters, $idx, $idx + 1 - $idx);
}
function fromBytes($bs) {
  global $char, $b, $c, $d, $z, $sub, $f, $val, $rem;
  $s = '';
  $i = 0;
  while ($i < count($bs)) {
  $s = $s . char($bs[$i]);
  $i = $i + 1;
};
  return $s;
}
$b = [98, 105, 110, 97, 114, 121];
echo rtrim(json_encode($b, 1344)), PHP_EOL;
$c = $b;
echo rtrim(json_encode($c, 1344)), PHP_EOL;
echo rtrim(json_encode($b == $c, 1344)), PHP_EOL;
$d = [];
$i = 0;
while ($i < count($b)) {
  $d = array_merge($d, [$b[$i]]);
  $i = $i + 1;
}
$d[1] = 97;
$d[4] = 110;
echo rtrim(fromBytes($b)), PHP_EOL;
echo rtrim(fromBytes($d)), PHP_EOL;
echo rtrim(json_encode(count($b) == 0, 1344)), PHP_EOL;
$z = array_merge($b, [122]);
echo rtrim(fromBytes($z)), PHP_EOL;
$sub = array_slice($b, 1, 3 - 1);
echo rtrim(fromBytes($sub)), PHP_EOL;
$f = [];
$i = 0;
while ($i < count($d)) {
  $val = $d[$i];
  if ($val == 110) {
  $f = array_merge($f, [109]);
} else {
  $f = array_merge($f, [$val]);
}
  $i = $i + 1;
}
echo rtrim(fromBytes($d) . ' -> ' . fromBytes($f)), PHP_EOL;
$rem = [];
$rem = array_merge($rem, [$b[0]]);
$i = 3;
while ($i < count($b)) {
  $rem = array_merge($rem, [$b[$i]]);
  $i = $i + 1;
}
echo rtrim(fromBytes($rem)), PHP_EOL;
