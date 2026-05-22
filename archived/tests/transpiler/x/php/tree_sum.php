<?php
function sum_tree($t) {
  return match($t) {
    $Leaf => 0,
    Node($left, $value, $right) => sum_tree($left) + $value + sum_tree($right),
};
}
$t = ["left" => $Leaf, "value" => 1, "right" => ["left" => $Leaf, "value" => 2, "right" => $Leaf]];
echo (is_float(sum_tree($t)) ? json_encode(sum_tree($t), 1344) : sum_tree($t)), PHP_EOL;
