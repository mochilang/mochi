<?php
function sum_tree($t) {
  return match($t) {
    $Leaf => 0,
    Node($left, $value, $right) => sum_tree($left) + $value + sum_tree($right),
};
}
$t = ["left" => $Leaf, "value" => 1, "right" => ["left" => $Leaf, "value" => 2, "right" => $Leaf]];
echo rtrim((is_float(sum_tree($t)) ? sprintf("%.15f", sum_tree($t)) : sum_tree($t))), PHP_EOL;
?>
