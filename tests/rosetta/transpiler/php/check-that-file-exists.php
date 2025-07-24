<?php
ini_set('memory_limit', '-1');
function printStat($fs, $path) {
  global $main;
  if (array_key_exists($path, $fs)) {
  if ($fs[$path]) {
  echo rtrim($path . ' is a directory'), PHP_EOL;
} else {
  echo rtrim($path . ' is a file'), PHP_EOL;
};
} else {
  echo rtrim('stat ' . $path . ': no such file or directory'), PHP_EOL;
}
}
function main() {
  global $printStat;
  $fs = [];
  $fs['docs'] = true;
  foreach (['input.txt', '/input.txt', 'docs', '/docs'] as $p) {
  printStat($fs, $p);
};
}
main();
