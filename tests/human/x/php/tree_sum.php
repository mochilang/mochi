<?php
class Leaf{}
class Node{public $left;public $value;public $right;function __construct($l,$v,$r){$this->left=$l;$this->value=$v;$this->right=$r;}}
function sum_tree($t){
    if($t instanceof Leaf) return 0;
    return sum_tree($t->left)+$t->value+sum_tree($t->right);
}
$t = new Node(new Leaf(),1,new Node(new Leaf(),2,new Leaf()));
var_dump(sum_tree($t));
?>
