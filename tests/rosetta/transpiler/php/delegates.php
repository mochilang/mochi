<?php
ini_set('memory_limit', '-1');
function operation($d) {
  global $a;
  if (isset($d['delegate']['thing'])) {
  return call_user_func($d['delegate']['thing']);
}
  return 'default implementation';
}
function newDelegate() {
  global $a;
  $m = [];
  $m['thing'] = function() use ($m) {
  return 'delegate implementation';
};
  return $m;
}
$a = ['delegate' => []];
echo rtrim(operation($a)), PHP_EOL;
$a['delegate'] = [];
echo rtrim(operation($a)), PHP_EOL;
$a['delegate'] = newDelegate();
echo rtrim(operation($a)), PHP_EOL;
