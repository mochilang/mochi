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
echo (is_float($math['sqrt'](16.0)) ? json_encode($math['sqrt'](16.0), 1344) : $math['sqrt'](16.0)), PHP_EOL;
echo (is_float($math["pi"]) ? json_encode($math["pi"], 1344) : $math["pi"]), PHP_EOL;
