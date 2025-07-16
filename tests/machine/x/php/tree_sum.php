<?php
abstract class Tree {}
class Leaf extends Tree {
}
class Node extends Tree {
    public $left;
    public $value;
    public $right;
    function __construct($left, $value, $right) {
        $this->left = $left;
        $this->value = $value;
        $this->right = $right;
    }
}
function sum_tree($t) {
    return (function($_t) {
    if ($_t instanceof Leaf) return 0;
    if ($_t instanceof Node) return (function($left, $value, $right) { return sum_tree($left) + $value + sum_tree($right); })($_t->left, $_t->value, $_t->right);
    return null;
})($t);
}
$t = new Node(new Leaf(), 1, new Node(new Leaf(), 2, new Leaf()));
_print(sum_tree($t));
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
