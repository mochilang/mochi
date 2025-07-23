<?php
$msg = "Hello World! ";
$shift = 0;
$inc = 1;
$clicks = 0;
$frames = 0;
while ($clicks < 5) {
  $line = "";
  $i = 0;
  while ($i < strlen($msg)) {
  $idx = ($shift + $i) % strlen($msg);
  $line = $line . substr($msg, $idx, $idx + 1 - $idx);
  $i = $i + 1;
};
  echo $line, PHP_EOL;
  $shift = ($shift + $inc) % strlen($msg);
  $frames = $frames + 1;
  if ($frames % strlen($msg) == 0) {
  $inc = strlen($msg) - $inc;
  $clicks = $clicks + 1;
}
}
