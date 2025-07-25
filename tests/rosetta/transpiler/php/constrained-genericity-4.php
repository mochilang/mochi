<?php
ini_set('memory_limit', '-1');
function peelFirstEat($p) {
  echo rtrim('mm, that ' . $p['value'] . ' was good!'), PHP_EOL;
}
function main() {
  $fb = ['items' => [['value' => 'banana'], ['value' => 'mango']]];
  $f0 = $fb['items'][0];
  peelFirstEat($f0);
}
main();
