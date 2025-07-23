<?php
function listStr($xs) {
  global $a, $cap_s;
  $s = "[";
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . json_encode($xs[$i], 1344);
  if ($i + 1 < count($xs)) {
  $s = $s . " ";
}
  $i = $i + 1;
};
  $s = $s . "]";
  return $s;
}
$a = [0, 0, 0, 0, 0];
echo "len(a) = " . json_encode(count($a), 1344), PHP_EOL;
echo "a = " . listStr($a), PHP_EOL;
$a[0] = 3;
echo "a = " . listStr($a), PHP_EOL;
echo "a[0] = " . json_encode($a[0], 1344), PHP_EOL;
$s = array_slice($a, 0, 4 - 0);
$cap_s = 5;
echo "s = " . listStr($s), PHP_EOL;
echo "len(s) = " . json_encode(count($s), 1344) . "  cap(s) = " . json_encode($cap_s, 1344), PHP_EOL;
$s = array_slice($a, 0, 5 - 0);
echo "s = " . listStr($s), PHP_EOL;
$a[0] = 22;
$s[0] = 22;
echo "a = " . listStr($a), PHP_EOL;
echo "s = " . listStr($s), PHP_EOL;
$s = array_merge($s, [4]);
$s = array_merge($s, [5]);
$s = array_merge($s, [6]);
$cap_s = 10;
echo "s = " . listStr($s), PHP_EOL;
echo "len(s) = " . json_encode(count($s), 1344) . "  cap(s) = " . json_encode($cap_s, 1344), PHP_EOL;
$a[4] = -1;
echo "a = " . listStr($a), PHP_EOL;
echo "s = " . listStr($s), PHP_EOL;
$s = [];
for ($i = 0; $i < 8; $i++) {
  $s = array_merge($s, [0]);
}
$cap_s = 8;
echo "s = " . listStr($s), PHP_EOL;
echo "len(s) = " . json_encode(count($s), 1344) . "  cap(s) = " . json_encode($cap_s, 1344), PHP_EOL;
