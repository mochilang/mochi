<?php
ini_set('memory_limit', '-1');
$lockExists = false;
function startOnce() {
  global $lockExists;
  if ($lockExists) {
  echo rtrim('an instance is already running'), PHP_EOL;
} else {
  $lockExists = true;
  echo rtrim('single instance started'), PHP_EOL;
}
}
function main() {
  global $lockExists;
  startOnce();
  startOnce();
}
main();
