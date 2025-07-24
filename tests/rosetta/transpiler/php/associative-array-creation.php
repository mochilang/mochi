<?php
ini_set('memory_limit', '-1');
function removeKey($m, $k) {
  global $main;
  $out = [];
  foreach (array_keys($m) as $key) {
  if ($key != $k) {
  $out[$key] = $m[$key];
}
};
  return $out;
}
function main() {
  global $removeKey;
  $x = null;
  $x = [];
  $x['foo'] = 3;
  $y1 = $x['bar'];
  $ok = array_key_exists('bar', $x);
  echo rtrim(json_encode($y1, 1344)), PHP_EOL;
  echo rtrim(($ok ? 'true' : 'false')), PHP_EOL;
  $x = removeKey($x, 'foo');
  $x = ['foo' => 2, 'bar' => 42, 'baz' => -1];
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($x['foo'], 1344))))))) . " " . rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($x['bar'], 1344))))))) . " " . rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($x['baz'], 1344))))))), PHP_EOL;
}
main();
