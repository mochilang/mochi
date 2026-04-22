<?php
class TreeNode {
    public int $val;
    public ?TreeNode $left = null;
    public ?TreeNode $right = null;
    public function __construct(int $val) { $this->val = $val; }
}

function serializeTree(?TreeNode $root): string {
    if ($root === null) return '[]';
    $out = [];
    $q = [$root];
    while ($q) {
        $node = array_shift($q);
        if ($node === null) {
            $out[] = 'null';
        } else {
            $out[] = strval($node->val);
            $q[] = $node->left;
            $q[] = $node->right;
        }
    }
    while ($out && end($out) === 'null') array_pop($out);
    return '[' . implode(',', $out) . ']';
}

function deserializeTree(string $data): ?TreeNode {
    if ($data === '[]') return null;
    $vals = explode(',', substr($data, 1, -1));
    $root = new TreeNode(intval($vals[0]));
    $q = [$root];
    $i = 1;
    while ($q && $i < count($vals)) {
        $node = array_shift($q);
        if ($i < count($vals) && $vals[$i] !== 'null') {
            $node->left = new TreeNode(intval($vals[$i]));
            $q[] = $node->left;
        }
        $i++;
        if ($i < count($vals) && $vals[$i] !== 'null') {
            $node->right = new TreeNode(intval($vals[$i]));
            $q[] = $node->right;
        }
        $i++;
    }
    return $root;
}

$lines = array_values(array_filter(array_map('trim', file('php://stdin')), 'strlen'));
if (!$lines) exit(0);
$t = intval($lines[0]);
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $out[] = serializeTree(deserializeTree($lines[$tc + 1]));
}
echo implode("\n\n", $out);
