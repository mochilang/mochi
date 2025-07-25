<?php
ini_set('memory_limit', '-1');
while (true) {
  $line = trim(fgets(STDIN));
  if ($line == '') {
  break;
}
  echo rtrim($line), PHP_EOL;
}
