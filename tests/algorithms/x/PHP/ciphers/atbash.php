<?php
ini_set('memory_limit', '-1');
function index_of($s, $c) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $c) {
  return $i;
}
  $i = $i + 1;
};
  return (-1);
}
function atbash($sequence) {
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower_rev = 'zyxwvutsrqponmlkjihgfedcba';
  $upper_rev = 'ZYXWVUTSRQPONMLKJIHGFEDCBA';
  $result = '';
  $i = 0;
  while ($i < strlen($sequence)) {
  $ch = substr($sequence, $i, $i + 1 - $i);
  $idx = index_of($lower, $ch);
  if ($idx != (-1)) {
  $result = $result . substr($lower_rev, $idx, $idx + 1 - $idx);
} else {
  $idx2 = index_of($upper, $ch);
  if ($idx2 != (-1)) {
  $result = $result . substr($upper_rev, $idx2, $idx2 + 1 - $idx2);
} else {
  $result = $result . $ch;
};
}
  $i = $i + 1;
};
  return $result;
}
echo rtrim(atbash('ABCDEFGH')), PHP_EOL;
echo rtrim(atbash('123GGjj')), PHP_EOL;
echo rtrim(atbash('testStringtest')), PHP_EOL;
echo rtrim(atbash('with space')), PHP_EOL;
