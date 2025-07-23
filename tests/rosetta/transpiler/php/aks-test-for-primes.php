<?php
function poly($p) {
  global $aks, $main;
  $s = "";
  $coef = 1;
  $i = $p;
  if ($coef != 1) {
  $s = $s . json_encode($coef, 1344);
}
  while ($i > 0) {
  $s = $s . "x";
  if ($i != 1) {
  $s = $s . "^" . json_encode($i, 1344);
}
  $coef = intval(($coef * $i / ($p - $i + 1)));
  $d = $coef;
  if (($p - ($i - 1)) % 2 == 1) {
  $d = -$d;
}
  if ($d < 0) {
  $s = $s . " - " . json_encode(-$d, 1344);
} else {
  $s = $s . " + " . json_encode($d, 1344);
}
  $i = $i - 1;
};
  if ($s == "") {
  $s = "1";
}
  return $s;
}
function aks($n) {
  global $poly, $main;
  if ($n < 2) {
  return false;
}
  $c = $n;
  $i = 1;
  while ($i < $n) {
  if ($c % $n != 0) {
  return false;
}
  $c = intval(($c * ($n - $i) / ($i + 1)));
  $i = $i + 1;
};
  return true;
}
function main() {
  global $poly, $aks;
  $p = 0;
  while ($p <= 7) {
  echo json_encode($p, 1344) . ":  " . poly($p), PHP_EOL;
  $p = $p + 1;
};
  $first = true;
  $p = 2;
  $line = "";
  while ($p < 50) {
  if (aks($p)) {
  if ($first) {
  $line = $line . json_encode($p, 1344);
  $first = false;
} else {
  $line = $line . " " . json_encode($p, 1344);
};
}
  $p = $p + 1;
};
  echo $line, PHP_EOL;
}
main();
