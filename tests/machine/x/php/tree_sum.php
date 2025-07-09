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
var_dump(sum_tree($t));
