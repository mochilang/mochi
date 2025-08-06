<?php
ini_set('memory_limit', '-1');
function send_file($content, $chunk_size) {
  $start = 0;
  $n = strlen($content);
  while ($start < $n) {
  $end = $start + $chunk_size;
  if ($end > $n) {
  $end = $n;
}
  $chunk = substr($content, $start, $end - $start);
  echo rtrim($chunk), PHP_EOL;
  $start = $end;
};
}
send_file('The quick brown fox jumps over the lazy dog.', 10);
