<?php
ini_set('memory_limit', '-1');
function newFactory() {
  global $funcs;
  $sn = 0;
  $New = function() use (&$New, $sn) {
  $sn = $sn + 1;
  $b = ['secret' => $sn];
  if ($sn == 1) {
  $b['Contents'] = 'rabbit';
} else {
  if ($sn == 2) {
  $b['Contents'] = 'rock';
};
}
  return $b;
};
  $Count = function() use (&$Count, $sn) {
  return $sn;
};
  return [$New, $Count];
}
$funcs = newFactory();
$New = $funcs[0];
$Count = $funcs[1];
