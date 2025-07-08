<?php
/**
 * @param Tree $t
 * @return int
 */
function mochi_sum_tree($t) {
	return (function($_t) {
	if ($_t instanceof Leaf) return 0;
	if ($_t instanceof Node) return (function($left, $value, $right) { return ((is_array(((is_array(mochi_sum_tree($left)) && is_array($value)) ? array_merge(mochi_sum_tree($left), $value) : ((is_string(mochi_sum_tree($left)) || is_string($value)) ? (mochi_sum_tree($left) . $value) : (mochi_sum_tree($left) + $value)))) && is_array(mochi_sum_tree($right))) ? array_merge(((is_array(mochi_sum_tree($left)) && is_array($value)) ? array_merge(mochi_sum_tree($left), $value) : ((is_string(mochi_sum_tree($left)) || is_string($value)) ? (mochi_sum_tree($left) . $value) : (mochi_sum_tree($left) + $value))), mochi_sum_tree($right)) : ((is_string(((is_array(mochi_sum_tree($left)) && is_array($value)) ? array_merge(mochi_sum_tree($left), $value) : ((is_string(mochi_sum_tree($left)) || is_string($value)) ? (mochi_sum_tree($left) . $value) : (mochi_sum_tree($left) + $value)))) || is_string(mochi_sum_tree($right))) ? (((is_array(mochi_sum_tree($left)) && is_array($value)) ? array_merge(mochi_sum_tree($left), $value) : ((is_string(mochi_sum_tree($left)) || is_string($value)) ? (mochi_sum_tree($left) . $value) : (mochi_sum_tree($left) + $value))) . mochi_sum_tree($right)) : (((is_array(mochi_sum_tree($left)) && is_array($value)) ? array_merge(mochi_sum_tree($left), $value) : ((is_string(mochi_sum_tree($left)) || is_string($value)) ? (mochi_sum_tree($left) . $value) : (mochi_sum_tree($left) + $value))) + mochi_sum_tree($right)))); })($_t->left, $_t->value, $_t->right);
	return null;
})($t);
}

class Leaf {
	public function __construct() {}
}

class Node {
	public $left;
	public $value;
	public $right;
	public function __construct($fields = []) {
		$this->left = $fields['left'] ?? null;
		$this->value = $fields['value'] ?? null;
		$this->right = $fields['right'] ?? null;
	}
}


// t: Node
$t = new Node(['left' => new Leaf(), 'value' => 1, 'right' => new Node(['left' => new Leaf(), 'value' => 2, 'right' => new Leaf()])]);
_print(mochi_sum_tree($t));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
