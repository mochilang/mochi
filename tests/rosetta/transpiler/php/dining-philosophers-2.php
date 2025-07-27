<?php
ini_set('memory_limit', '-1');
function main() {
  $philosophers = ['Aristotle', 'Kant', 'Spinoza', 'Marx', 'Russell'];
  $hunger = 3;
  echo rtrim('table empty'), PHP_EOL;
  foreach ($philosophers as $p) {
  echo rtrim($p . ' seated'), PHP_EOL;
};
  $i = 0;
  while ($i < count($philosophers)) {
  $name = $philosophers[$i];
  $h = 0;
  while ($h < $hunger) {
  echo rtrim($name . ' hungry'), PHP_EOL;
  echo rtrim($name . ' eating'), PHP_EOL;
  echo rtrim($name . ' thinking'), PHP_EOL;
  $h = $h + 1;
};
  echo rtrim($name . ' satisfied'), PHP_EOL;
  echo rtrim($name . ' left the table'), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim('table empty'), PHP_EOL;
}
main();
