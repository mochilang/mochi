<?php
ini_set('memory_limit', '-1');
function removeName($names, $name) {
  global $main;
  $out = [];
  foreach ($names as $n) {
  if ($n != $name) {
  $out = array_merge($out, [$n]);
}
};
  return $out;
}
function main() {
  global $removeName;
  $clients = [];
  $broadcast = function($msg) use (&$broadcast, $clients, &$removeName) {
  echo rtrim($msg), PHP_EOL;
};
  $add = function($name) use (&$add, $clients, $broadcast, &$removeName) {
  $clients = array_merge($clients, [$name]);
  $broadcast('+++ "' . $name . '" connected +++
');
};
  $send = function($name, $msg) use (&$send, $clients, $broadcast, $add, &$removeName) {
  $broadcast($name . '> ' . $msg . '
');
};
  $remove = function($name) use (&$remove, $clients, $broadcast, $add, $send, &$removeName) {
  $clients = removeName($clients, $name);
  $broadcast('--- "' . $name . '" disconnected ---
');
};
  $add('Alice');
  $add('Bob');
  $send('Alice', 'Hello Bob!');
  $send('Bob', 'Hi Alice!');
  $remove('Bob');
  $remove('Alice');
  $broadcast('Server stopping!
');
}
main();
