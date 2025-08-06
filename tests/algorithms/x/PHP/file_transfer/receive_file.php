<?php
ini_set('memory_limit', '-1');
function receive_file($chunks) {
  $out = '';
  $i = 0;
  echo rtrim('File opened'), PHP_EOL;
  echo rtrim('Receiving data...'), PHP_EOL;
  while ($i < count($chunks)) {
  $data = $chunks[$i];
  if ($data == '') {
  break;
}
  $out = $out . $data;
  $i = $i + 1;
};
  echo rtrim('Successfully received the file'), PHP_EOL;
  echo rtrim('Connection closed'), PHP_EOL;
  return $out;
}
function main() {
  $incoming = ['Hello ', 'from ', 'server'];
  $received = receive_file($incoming);
  echo rtrim($received), PHP_EOL;
}
main();
