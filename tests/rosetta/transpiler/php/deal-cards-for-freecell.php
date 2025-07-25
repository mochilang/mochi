<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$seed = 1;
function rnd() {
  global $seed, $suits, $nums;
  $seed = ($seed * 214013 + 2531011) % 2147483648;
  return _intdiv($seed, 65536);
}
function deal($game) {
  global $seed, $suits, $nums;
  $seed = $game;
  $deck = [];
  $i = 0;
  while ($i < 52) {
  $deck = array_merge($deck, [51 - $i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < 51) {
  $j = 51 - (rnd() % (52 - $i));
  $tmp = $deck[$i];
  $deck[$i] = $deck[$j];
  $deck[$j] = $tmp;
  $i = $i + 1;
};
  return $deck;
}
$suits = 'CDHS';
$nums = 'A23456789TJQK';
function show($cards) {
  global $seed, $suits, $nums;
  $i = 0;
  while ($i < count($cards)) {
  $c = $cards[$i];
  fwrite(STDOUT, ' ' . substr($nums, _intdiv($c, 4), _intdiv($c, 4) + 1 - _intdiv($c, 4)) . substr($suits, $c % 4, $c % 4 + 1 - $c % 4));
  if (($i + 1) % 8 == 0 || $i + 1 == count($cards)) {
  echo rtrim(''), PHP_EOL;
}
  $i = $i + 1;
};
}
echo rtrim(''), PHP_EOL;
echo rtrim('Game #1'), PHP_EOL;
show(deal(1));
echo rtrim(''), PHP_EOL;
echo rtrim('Game #617'), PHP_EOL;
show(deal(617));
