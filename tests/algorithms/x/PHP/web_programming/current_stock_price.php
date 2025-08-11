<?php
ini_set('memory_limit', '-1');
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function find($text, $pattern, $start) {
  $i = $start;
  $limit = strlen($text) - strlen($pattern);
  while ($i <= $limit) {
  if (substr($text, $i, _iadd($i, strlen($pattern)) - $i) == $pattern) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function stock_price($symbol) {
  $pages = ['AAPL' => '<span data-testid="qsp-price">228.43</span>', 'AMZN' => '<span data-testid="qsp-price">201.85</span>', 'IBM' => '<span data-testid="qsp-price">210.30</span>', 'GOOG' => '<span data-testid="qsp-price">177.86</span>', 'MSFT' => '<span data-testid="qsp-price">414.82</span>', 'ORCL' => '<span data-testid="qsp-price">188.87</span>'];
  if (array_key_exists($symbol, $pages)) {
  $html = $pages[$symbol];
  $marker = '<span data-testid="qsp-price">';
  $start_idx = find($html, $marker, 0);
  if ($start_idx != (-1)) {
  $price_start = _iadd($start_idx, strlen($marker));
  $end_idx = find($html, '</span>', $price_start);
  if ($end_idx != (-1)) {
  return substr($html, $price_start, $end_idx - $price_start);
};
};
}
  return 'No <fin-streamer> tag with the specified data-testid attribute found.';
}
foreach (['AAPL', 'AMZN', 'IBM', 'GOOG', 'MSFT', 'ORCL'] as $symbol) {
  echo rtrim('Current ' . $symbol . ' stock price is ' . stock_price($symbol)), PHP_EOL;
}
