<?php
ini_set('memory_limit', '-1');
function padLeft($s, $w) {
  global $dna, $le, $i, $k, $a, $c, $g, $t, $idx, $ch;
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
}
$dna = '' . 'CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG' . 'CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG' . 'AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT' . 'GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT' . 'CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG' . 'TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA' . 'TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT' . 'CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG' . 'TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC' . 'GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT';
echo rtrim('SEQUENCE:'), PHP_EOL;
$le = strlen($dna);
$i = 0;
while ($i < $le) {
  $k = $i + 50;
  if ($k > $le) {
  $k = $le;
}
  echo rtrim(padLeft(json_encode($i, 1344), 5) . ': ' . substr($dna, $i, $k - $i)), PHP_EOL;
  $i = $i + 50;
}
$a = 0;
$c = 0;
$g = 0;
$t = 0;
$idx = 0;
while ($idx < $le) {
  $ch = substr($dna, $idx, $idx + 1 - $idx);
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
  $idx = $idx + 1;
}
echo rtrim(''), PHP_EOL;
echo rtrim('BASE COUNT:'), PHP_EOL;
echo rtrim('    A: ' . padLeft(json_encode($a, 1344), 3)), PHP_EOL;
echo rtrim('    C: ' . padLeft(json_encode($c, 1344), 3)), PHP_EOL;
echo rtrim('    G: ' . padLeft(json_encode($g, 1344), 3)), PHP_EOL;
echo rtrim('    T: ' . padLeft(json_encode($t, 1344), 3)), PHP_EOL;
echo rtrim('    ------'), PHP_EOL;
echo rtrim('    Σ: ' . json_encode($le, 1344)), PHP_EOL;
echo rtrim('    ======'), PHP_EOL;
