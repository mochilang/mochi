<?php
function pow10($exp) {
  global $totient, $pps, $getPerfectPowers, $getAchilles, $sortInts, $pad, $main;
  $n = 1;
  $i = 0;
  while ($i < $exp) {
  $n = $n * 10;
  $i = $i + 1;
};
  return $n;
}
function totient($n) {
  global $pow10, $pps, $getPerfectPowers, $getAchilles, $sortInts, $pad, $main;
  $tot = $n;
  $nn = $n;
  $i = 2;
  while ($i * $i <= $nn) {
  if ($nn % $i == 0) {
  while ($nn % $i == 0) {
  $nn = intdiv($nn, $i);
};
  $tot = $tot - intdiv($tot, $i);
}
  if ($i == 2) {
  $i = 1;
}
  $i = $i + 2;
};
  if ($nn > 1) {
  $tot = $tot - intdiv($tot, $nn);
}
  return $tot;
}
$pps = [];
function getPerfectPowers($maxExp) {
  global $pow10, $totient, $pps, $getAchilles, $sortInts, $pad, $main;
  $upper = pow10($maxExp);
  $i = 2;
  while ($i * $i < $upper) {
  $p = $i;
  while (true) {
  $p = $p * $i;
  if ($p >= $upper) {
  break;
}
  $pps[$p] = true;
};
  $i = $i + 1;
};
}
function getAchilles($minExp, $maxExp) {
  global $pow10, $totient, $pps, $getPerfectPowers, $sortInts, $pad, $main;
  $lower = pow10($minExp);
  $upper = pow10($maxExp);
  $achilles = [];
  $b = 1;
  while ($b * $b * $b < $upper) {
  $b3 = $b * $b * $b;
  $a = 1;
  while (true) {
  $p = $b3 * $a * $a;
  if ($p >= $upper) {
  break;
}
  if ($p >= $lower) {
  if (!(array_key_exists($p, $pps))) {
  $achilles[$p] = true;
};
}
  $a = $a + 1;
};
  $b = $b + 1;
};
  return $achilles;
}
function sortInts(&$xs) {
  global $pow10, $totient, $pps, $getPerfectPowers, $getAchilles, $pad, $main;
  $res = [];
  $tmp = $xs;
  while (count($tmp) > 0) {
  $min = $tmp[0];
  $idx = 0;
  $i = 1;
  while ($i < count($tmp)) {
  if ($tmp[$i] < $min) {
  $min = $tmp[$i];
  $idx = $i;
}
  $i = $i + 1;
};
  $res = array_merge($res, [$min]);
  $out = [];
  $j = 0;
  while ($j < count($tmp)) {
  if ($j != $idx) {
  $out = array_merge($out, [$tmp[$j]]);
}
  $j = $j + 1;
};
  $tmp = $out;
};
  return $res;
}
function pad($n, $width) {
  global $pow10, $totient, $pps, $getPerfectPowers, $getAchilles, $sortInts, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < $width) {
  $s = " " . $s;
};
  return $s;
}
function main() {
  global $pow10, $totient, $pps, $getPerfectPowers, $getAchilles, $sortInts, $pad;
  $maxDigits = 15;
  getPerfectPowers(5);
  $achSet = getAchilles(1, 5);
  $ach = [];
  foreach (array_keys($achSet) as $k) {
  $ach = array_merge($ach, [$k]);
};
  $ach = sortInts($ach);
  echo "First 50 Achilles numbers:", PHP_EOL;
  $i = 0;
  while ($i < 50) {
  $line = "";
  $j = 0;
  while ($j < 10) {
  $line = $line . pad($ach[$i], 4);
  if ($j < 9) {
  $line = $line . " ";
}
  $i = $i + 1;
  $j = $j + 1;
};
  echo $line, PHP_EOL;
};
  echo "\nFirst 30 strong Achilles numbers:", PHP_EOL;
  $strong = [];
  $count = 0;
  $idx = 0;
  while ($count < 30) {
  $tot = totient($ach[$idx]);
  if (array_key_exists($tot, $achSet)) {
  $strong = array_merge($strong, [$ach[$idx]]);
  $count = $count + 1;
}
  $idx = $idx + 1;
};
  $i = 0;
  while ($i < 30) {
  $line = "";
  $j = 0;
  while ($j < 10) {
  $line = $line . pad($strong[$i], 5);
  if ($j < 9) {
  $line = $line . " ";
}
  $i = $i + 1;
  $j = $j + 1;
};
  echo $line, PHP_EOL;
};
  echo "\nNumber of Achilles numbers with:", PHP_EOL;
  $counts = [1, 12, 47, 192, 664, 2242, 7395, 24008, 77330, 247449, 788855, 2508051, 7960336, 25235383];
  $d = 2;
  while ($d <= $maxDigits) {
  $c = $counts[$d - 2];
  echo pad($d, 2) . " digits: " . json_encode($c, 1344), PHP_EOL;
  $d = $d + 1;
};
}
main();
