<?php
ini_set('memory_limit', '-1');
function quibble($items) {
  $n = count($items);
  if ($n == 0) {
  return '{}';
} else {
  if ($n == 1) {
  return '{' . $items[0] . '}';
} else {
  if ($n == 2) {
  return '{' . $items[0] . ' and ' . $items[1] . '}';
} else {
  $prefix = '';
  for ($i = 0; $i < $n - 1; $i++) {
  if ($i == $n - 1) {
  break;
}
  if ($i > 0) {
  $prefix = $prefix . ', ';
}
  $prefix = $prefix . $items[$i];
};
  return '{' . $prefix . ' and ' . $items[$n - 1] . '}';
};
};
}
}
function main() {
  echo rtrim(quibble([])), PHP_EOL;
  echo rtrim(quibble(['ABC'])), PHP_EOL;
  echo rtrim(quibble(['ABC', 'DEF'])), PHP_EOL;
  echo rtrim(quibble(['ABC', 'DEF', 'G', 'H'])), PHP_EOL;
}
main();
