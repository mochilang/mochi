<?php
$boom = function($a, $b) {
  echo rtrim("boom"), PHP_EOL;
  return true;
};
echo rtrim((false && $boom(1, 2) ? "true" : "false")), PHP_EOL;
echo rtrim((true || $boom(1, 2) ? "true" : "false")), PHP_EOL;
?>
