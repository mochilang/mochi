// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:34:19Z
Map<String, any> Node(int data) {
  return {
    'Data': data,
    'Balance': 0,
    'Link': [null, null],
  };
}

any getLink(Map<String, any> n, int dir) {
  return (((n as Map)['Link'] as List<any>))[dir];
}

void setLink(Map<String, any> n, int dir, any v) {
  var links = ((n as Map)['Link'] as List<any>);
  links[dir] = v;
  n['Link'] = links;
}

int opp(int dir) {
  return 1 - dir;
}

Map<String, any> single(Map<String, any> root, int dir) {
  var tmp = getLink(root, opp(dir));
  setLink(root, opp(dir), getLink(tmp, dir));
  setLink(tmp, dir, root);
  return tmp;
}

Map<String, any> double(Map<String, any> root, int dir) {
  var tmp = getLink(getLink(root, opp(dir)), dir);
  setLink(getLink(root, opp(dir)), dir, getLink(tmp, opp(dir)));
  setLink(tmp, opp(dir), getLink(root, opp(dir)));
  setLink(root, opp(dir), tmp);
  tmp = getLink(root, opp(dir));
  setLink(root, opp(dir), getLink(tmp, dir));
  setLink(tmp, dir, root);
  return tmp;
}

void adjustBalance(Map<String, any> root, int dir, int bal) {
  var n = (getLink(root, dir) as Map<String, any>);
  var nn = (getLink(n, opp(dir)) as Map<String, any>);
  if (nn['Balance'] == 0) {
    root['Balance'] = 0;
    n['Balance'] = 0;
  }
  else 
  if (nn['Balance'] == bal) {
    root['Balance'] = -(bal as num);
    n['Balance'] = 0;
  }
  else {
    root['Balance'] = 0;
    n['Balance'] = bal;
  }
  nn['Balance'] = 0;
}

Map<String, any> insertBalance(Map<String, any> root, int dir) {
  var n = (getLink(root, dir) as Map<String, any>);
  var bal = 2 * dir - 1;
  if (n['Balance'] == bal) {
    root['Balance'] = 0;
    n['Balance'] = 0;
    return single(root, opp(dir));
  }
  adjustBalance(root, dir, bal);
  return double(root, opp(dir));
}

Map<String, any> insertR(any root, int data) {
  if (root == null) {
    return {'node': Node(data), 'done': false};
  }
  var node = (root as Map<String, any>);
  num dir = 0;
  if ((int.parse(node['Data'])) < data) {
    dir = 1;
  }
  var r = insertR(getLink(node, dir), data);
  setLink(node, dir, r['node']);
  if (r['done'] != null) {
    return {'node': node, 'done': true};
  }
  node['Balance'] = (int.parse(node['Balance'])) + (((2 * (dir as num) as num) - 1) as num);
  if (node['Balance'] == 0) {
    return {'node': node, 'done': true};
  }
  if (node['Balance'] == 1 || node['Balance'] == (-1)) {
    return {'node': node, 'done': false};
  }
  return {
    'node': insertBalance(node, dir),
    'done': true,
  };
}

any Insert(any tree, int data) {
  var r = insertR(tree, data);
  return r['node'];
}

Map<String, any> removeBalance(Map<String, any> root, int dir) {
  var n = (getLink(root, opp(dir)) as Map<String, any>);
  var bal = 2 * dir - 1;
  if (n['Balance'] == (-(bal as num))) {
    root['Balance'] = 0;
    n['Balance'] = 0;
    return {
      'node': single(root, dir),
      'done': false,
    };
  }
  if (n['Balance'] == bal) {
    adjustBalance(root, opp(dir), (-(bal as num)));
    return {
      'node': double(root, dir),
      'done': false,
    };
  }
  root['Balance'] = -(bal as num);
  n['Balance'] = bal;
  return {
    'node': single(root, dir),
    'done': true,
  };
}

Map<String, any> removeR(any root, int data) {
  if (root == null) {
    return {'node': null, 'done': false};
  }
  var node = (root as Map<String, any>);
  if ((int.parse(node['Data'])) == data) {
    if (getLink(node, 0) == null) {
      return {
        'node': getLink(node, 1),
        'done': false,
      };
    }
    if (getLink(node, 1) == null) {
      return {
        'node': getLink(node, 0),
        'done': false,
      };
    }
    var heir = getLink(node, 0);
    while (getLink(heir, 1) != null) {
      heir = getLink(heir, 1);
    }
    node['Data'] = heir['Data'];
    data = int.parse(heir['Data']);
  }
  num dir = 0;
  if ((int.parse(node['Data'])) < data) {
    dir = 1;
  }
  var r = removeR(getLink(node, dir), data);
  setLink(node, dir, r['node']);
  if (r['done'] != null) {
    return {'node': node, 'done': true};
  }
  node['Balance'] = (int.parse(node['Balance'])) + 1 - (2 * (dir as num) as num);
  if (node['Balance'] == 1 || node['Balance'] == (-1)) {
    return {'node': node, 'done': true};
  }
  if (node['Balance'] == 0) {
    return {'node': node, 'done': false};
  }
  return removeBalance(node, dir);
}

any Remove(any tree, int data) {
  var r = removeR(tree, data);
  return r['node'];
}

String indentStr(int n) {
  var s = '';
  num i = 0;
  while ((i as num) < n) {
    s = s + ' ';
    i = (i as num) + 1;
  }
  return s;
}

void dumpNode(any node, int indent, bool comma) {
  var sp = indentStr(indent);
  if (node == null) {
    var line = sp + 'null';
    if (comma) {
      line = line + ',';
    }
    print(line);
  }
  else {
    print(sp + '{');
    print(indentStr(indent + 3) + 'Data": ' + node['Data'].toString() + ',');
    print(indentStr(indent + 3) + 'Balance": ' + node['Balance'].toString() + ',');
    print(indentStr(indent + 3) + 'Link": [');
    dumpNode(getLink(node, 0), indent + 6, true);
    dumpNode(getLink(node, 1), indent + 6, false);
    print(indentStr(indent + 3) + ']');
    var end = sp + '}';
    if (comma) {
      end = end + ',';
    }
    print(end);
  }
}

void dump(any node, int indent) {
  dumpNode(node, indent, false);
}

void _main() {
  var tree = null;
  print('Empty tree:');
  dump(tree, 0);
  print('');
  print('Insert test:');
  tree = Insert(tree, 3);
  tree = Insert(tree, 1);
  tree = Insert(tree, 4);
  tree = Insert(tree, 1);
  tree = Insert(tree, 5);
  dump(tree, 0);
  print('');
  print('Remove test:');
  tree = Remove(tree, 3);
  tree = Remove(tree, 1);
  var t = (tree as Map<String, any>);
  t['Balance'] = 0;
  tree = t;
  dump(tree, 0);
}

void main() {
  _main();
}
