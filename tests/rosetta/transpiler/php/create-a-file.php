<?php
ini_set('memory_limit', '-1');
function createFile(&$fs, $fn) {
  if (array_key_exists($fn, $fs)) {
  echo rtrim('open ' . $fn . ': file exists'), PHP_EOL;
} else {
  $fs[$fn] = false;
  echo rtrim('file ' . $fn . ' created!'), PHP_EOL;
}
}
function createDir(&$fs, $dn) {
  if (array_key_exists($dn, $fs)) {
  echo rtrim('mkdir ' . $dn . ': file exists'), PHP_EOL;
} else {
  $fs[$dn] = true;
  echo rtrim('directory ' . $dn . ' created!'), PHP_EOL;
}
}
function main() {
  $fs = [];
  $fs['docs'] = true;
  createFile($fs, 'input.txt');
  createFile($fs, '/input.txt');
  createDir($fs, 'docs');
  createDir($fs, '/docs');
}
main();
