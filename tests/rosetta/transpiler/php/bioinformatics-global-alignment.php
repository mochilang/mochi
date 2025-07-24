<?php
ini_set('memory_limit', '-1');
function padLeft($s, $w) {
  global $indexOfFrom, $containsStr, $distinct, $permutations, $headTailOverlap, $deduplicate, $joinAll, $shortestCommonSuperstring, $printCounts, $main;
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
}
function indexOfFrom($s, $ch, $start) {
  global $padLeft, $containsStr, $distinct, $permutations, $headTailOverlap, $deduplicate, $joinAll, $shortestCommonSuperstring, $printCounts, $main;
  $i = $start;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function containsStr($s, $sub) {
  global $padLeft, $indexOfFrom, $distinct, $permutations, $headTailOverlap, $deduplicate, $joinAll, $shortestCommonSuperstring, $printCounts, $main;
  $i = 0;
  $sl = strlen($s);
  $subl = strlen($sub);
  while ($i <= $sl - $subl) {
  if (substr($s, $i, $i + $subl - $i) == $sub) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function distinct($slist) {
  global $padLeft, $indexOfFrom, $containsStr, $permutations, $headTailOverlap, $deduplicate, $joinAll, $shortestCommonSuperstring, $printCounts, $main;
  $res = [];
  foreach ($slist as $s) {
  $found = false;
  foreach ($res as $r) {
  if ($r == $s) {
  $found = true;
  break;
}
};
  if (!$found) {
  $res = array_merge($res, [$s]);
}
};
  return $res;
}
function permutations($xs) {
  global $padLeft, $indexOfFrom, $containsStr, $distinct, $headTailOverlap, $deduplicate, $joinAll, $shortestCommonSuperstring, $printCounts, $main;
  if (count($xs) <= 1) {
  return [$xs];
}
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $rest = [];
  $j = 0;
  while ($j < count($xs)) {
  if ($j != $i) {
  $rest = array_merge($rest, [$xs[$j]]);
}
  $j = $j + 1;
};
  $subs = permutations($rest);
  foreach ($subs as $p) {
  $perm = [$xs[$i]];
  $k = 0;
  while ($k < count($p)) {
  $perm = array_merge($perm, [$p[$k]]);
  $k = $k + 1;
};
  $res = array_merge($res, [$perm]);
};
  $i = $i + 1;
};
  return $res;
}
function headTailOverlap($s1, $s2) {
  global $padLeft, $indexOfFrom, $containsStr, $distinct, $permutations, $deduplicate, $joinAll, $shortestCommonSuperstring, $printCounts, $main;
  $start = 0;
  while (true) {
  $ix = indexOfFrom($s1, substr($s2, 0, 1 - 0), $start);
  if ($ix == 0 - 1) {
  return 0;
}
  $start = $ix;
  if (substr($s2, 0, strlen($s1) - $start - 0) == substr($s1, $start, strlen($s1) - $start)) {
  return strlen($s1) - $start;
}
  $start = $start + 1;
};
}
function deduplicate($slist) {
  global $padLeft, $indexOfFrom, $containsStr, $distinct, $permutations, $headTailOverlap, $joinAll, $shortestCommonSuperstring, $printCounts, $main;
  $arr = distinct($slist);
  $filtered = [];
  $i = 0;
  while ($i < count($arr)) {
  $s1 = $arr[$i];
  $within = false;
  $j = 0;
  while ($j < count($arr)) {
  if ($j != $i && containsStr($arr[$j], $s1)) {
  $within = true;
  break;
}
  $j = $j + 1;
};
  if (!$within) {
  $filtered = array_merge($filtered, [$s1]);
}
  $i = $i + 1;
};
  return $filtered;
}
function joinAll($ss) {
  global $padLeft, $indexOfFrom, $containsStr, $distinct, $permutations, $headTailOverlap, $deduplicate, $shortestCommonSuperstring, $printCounts, $main;
  $out = '';
  foreach ($ss as $s) {
  $out = $out . $s;
};
  return $out;
}
function shortestCommonSuperstring($slist) {
  global $padLeft, $indexOfFrom, $containsStr, $distinct, $permutations, $headTailOverlap, $deduplicate, $joinAll, $printCounts, $main;
  $ss = deduplicate($slist);
  $shortest = joinAll($ss);
  $perms = permutations($ss);
  $idx = 0;
  while ($idx < count($perms)) {
  $perm = $perms[$idx];
  $sup = $perm[0];
  $i = 0;
  while ($i < count($ss) - 1) {
  $ov = headTailOverlap($perm[$i], $perm[$i + 1]);
  $sup = $sup . substr($perm[$i + 1], $ov, strlen($perm[$i + 1]) - $ov);
  $i = $i + 1;
};
  if (strlen($sup) < strlen($shortest)) {
  $shortest = $sup;
}
  $idx = $idx + 1;
};
  return $shortest;
}
function printCounts($seq) {
  global $padLeft, $indexOfFrom, $containsStr, $distinct, $permutations, $headTailOverlap, $deduplicate, $joinAll, $shortestCommonSuperstring, $main;
  $a = 0;
  $c = 0;
  $g = 0;
  $t = 0;
  $i = 0;
  while ($i < strlen($seq)) {
  $ch = substr($seq, $i, $i + 1 - $i);
  if ($ch == 'A') {
  $a = $a + 1;
} else {
  if ($ch == 'C') {
  $c = $c + 1;
} else {
  if ($ch == 'G') {
  $g = $g + 1;
} else {
  if ($ch == 'T') {
  $t = $t + 1;
};
};
};
}
  $i = $i + 1;
};
  $total = strlen($seq);
  echo rtrim('
Nucleotide counts for ' . $seq . ':
'), PHP_EOL;
  echo rtrim(padLeft('A', 10) . padLeft(json_encode($a, 1344), 12)), PHP_EOL;
  echo rtrim(padLeft('C', 10) . padLeft(json_encode($c, 1344), 12)), PHP_EOL;
  echo rtrim(padLeft('G', 10) . padLeft(json_encode($g, 1344), 12)), PHP_EOL;
  echo rtrim(padLeft('T', 10) . padLeft(json_encode($t, 1344), 12)), PHP_EOL;
  echo rtrim(padLeft('Other', 10) . padLeft(json_encode($total - ($a + $c + $g + $t), 1344), 12)), PHP_EOL;
  echo rtrim('  ____________________'), PHP_EOL;
  echo rtrim(padLeft('Total length', 14) . padLeft(json_encode($total, 1344), 8)), PHP_EOL;
}
function main() {
  global $padLeft, $indexOfFrom, $containsStr, $distinct, $permutations, $headTailOverlap, $deduplicate, $joinAll, $shortestCommonSuperstring, $printCounts;
  $tests = [['TA', 'AAG', 'TA', 'GAA', 'TA'], ['CATTAGGG', 'ATTAG', 'GGG', 'TA'], ['AAGAUGGA', 'GGAGCGCAUC', 'AUCGCAAUAAGGA'], ['ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT', 'GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT', 'CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA', 'TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT', 'GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC', 'CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT', 'TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC', 'GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT', 'TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA', 'TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA']];
  foreach ($tests as $seqs) {
  $scs = shortestCommonSuperstring($seqs);
  printCounts($scs);
};
}
main();
