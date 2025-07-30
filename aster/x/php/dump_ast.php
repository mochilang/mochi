<?php
$code = file_get_contents('php://stdin');
$version = max(ast\get_supported_versions());
$ast = ast\parse_code($code, $version);
function convert($node) {
    if ($node instanceof ast\Node) {
        $res = [
            'kind' => ast\get_kind_name($node->kind),
            'flags' => $node->flags,
            'lineno' => $node->lineno,
        ];
        $children = [];
        foreach ($node->children as $k => $child) {
            $children[$k] = convert($child);
        }
        if ($children) {
            $res['children'] = $children;
        }
        return $res;
    }
    return $node;
}

echo json_encode(convert($ast), JSON_PRETTY_PRINT);
