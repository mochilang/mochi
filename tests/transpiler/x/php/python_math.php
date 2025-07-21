<?php
$math = ["sqrt" => function($x) {
  return sqrt($x);
}, "pow" => function($x, $y) {
  return pow($x, $y);
}, "sin" => function($x) {
  return sin($x);
}, "log" => function($x) {
  return log($x);
}, "pi" => M_PI, "e" => M_E];
$r = 3.0;
$area = $math["pi"] * $math['pow']($r, 2.0);
$root = $math['sqrt'](49.0);
$sin45 = $math['sin']($math["pi"] / 4.0);
$log_e = $math['log']($math["e"]);
echo rtrim("Circle area with r =" . " " . (is_float($r) ? json_encode($r, 1344) : $r) . " " . "=>" . " " . (is_float($area) ? json_encode($area, 1344) : $area)), PHP_EOL;
echo rtrim("Square root of 49:" . " " . (is_float($root) ? json_encode($root, 1344) : $root)), PHP_EOL;
echo rtrim("sin(π/4):" . " " . (is_float($sin45) ? json_encode($sin45, 1344) : $sin45)), PHP_EOL;
echo rtrim("log(e):" . " " . (is_float($log_e) ? json_encode($log_e, 1344) : $log_e)), PHP_EOL;
?>
