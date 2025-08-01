// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:33:21Z
var SIZE = 4;

List<List<int>> newBoard() {
  List<List<int>> b = [];
  num y = 0;
  while ((y as num) < SIZE) {
    List<int> row = [];
    num x = 0;
    while ((x as num) < SIZE) {
      row = List.from(row)..add(0);
      x = (x as num) + 1;
    }
    b = List.from(b)..add(row);
    y = (y as num) + 1;
  }
  return b;
}

Map<String, any> spawnTile(List<List<int>> b) {
  List<List<int>> empty = [];
  num y = 0;
  while ((y as num) < SIZE) {
    num x = 0;
    while ((x as num) < SIZE) {
      if (b[y][x] == 0) {
        empty = List.from(empty)..add([x, y]);
      }
      x = (x as num) + 1;
    }
    y = (y as num) + 1;
  }
  if (empty.length == 0) {
    return {'board': b, 'full': true};
  }
  var idx = DateTime.now().microsecondsSinceEpoch % empty.length;
  var cell = empty[idx];
  var val = 4;
  if (DateTime.now().microsecondsSinceEpoch % 10 < 9) {
    val = 2;
  }
  b[cell[1]][cell[0]] = val;
  return {
    'board': b,
    'full': empty.length == 1,
  };
}

String pad(int n) {
  var s = n.toString();
  var pad = 4 - s.length;
  num i = 0;
  var out = '';
  while ((i as num) < (pad as num)) {
    out = out + ' ';
    i = (i as num) + 1;
  }
  return (out as num) + (s as num);
}

void draw(List<List<int>> b, int score) {
  print('Score: ' + score.toString());
  num y = 0;
  while ((y as num) < SIZE) {
    print('+----+----+----+----+');
    var line = '|';
    num x = 0;
    while ((x as num) < SIZE) {
      var v = b[y][x];
      if (v == 0) {
        line = line + '    |';
      }
      else {
        line = line + pad(v) + '|';
      }
      x = (x as num) + 1;
    }
    print(line);
    y = (y as num) + 1;
  }
  print('+----+----+----+----+');
  print('W=Up S=Down A=Left D=Right Q=Quit');
}

List<int> reverseRow(List<int> r) {
  List<int> out = [];
  var i = r.length - 1;
  while ((i as num) >= 0) {
    out = List.from(out)..add(r[i]);
    i = (i as num) - 1;
  }
  return out;
}

Map<String, any> slideLeft(List<int> row) {
  List<int> xs = [];
  num i = 0;
  while ((i as num) < row.length) {
    if (row[i] != 0) {
      xs = List.from(xs)..add(row[i]);
    }
    i = (i as num) + 1;
  }
  List<int> res = [];
  num gain = 0;
  i = 0;
  while ((i as num) < xs.length) {
    if (((i as num) + 1 as num) < xs.length && xs[i] == xs[(i as num) + 1]) {
      var v = (xs[i] as num) * 2;
      gain = (gain as num) + (v as num);
      res = List.from(res)..add(v);
      i = (i as num) + 2;
    }
    else {
      res = List.from(res)..add(xs[i]);
      i = (i as num) + 1;
    }
  }
  while (res.length < SIZE) {
    res = List.from(res)..add(0);
  }
  return {'row': res, 'gain': gain};
}

Map<String, any> moveLeft(List<List<int>> b, int score) {
  var moved = false;
  num y = 0;
  while ((y as num) < SIZE) {
    var r = slideLeft(b[y]);
    var _new = (r as Map)['row'];
    score = score + ((r as Map)['gain'] as num);
    num x = 0;
    while ((x as num) < SIZE) {
      if (b[y][x] != _new[x]) {
        moved = true;
      }
      b[y][x] = _new[x];
      x = (x as num) + 1;
    }
    y = (y as num) + 1;
  }
  return {'board': b, 'score': score, 'moved': moved};
}

Map<String, any> moveRight(List<List<int>> b, int score) {
  var moved = false;
  num y = 0;
  while ((y as num) < SIZE) {
    var rev = reverseRow(b[y]);
    var r = slideLeft(rev);
    rev = (r as Map)['row'];
    score = score + ((r as Map)['gain'] as num);
    rev = reverseRow(rev);
    num x = 0;
    while ((x as num) < SIZE) {
      if (b[y][x] != rev[x]) {
        moved = true;
      }
      b[y][x] = rev[x];
      x = (x as num) + 1;
    }
    y = (y as num) + 1;
  }
  return {'board': b, 'score': score, 'moved': moved};
}

List<int> getCol(List<List<int>> b, int x) {
  List<int> col = [];
  num y = 0;
  while ((y as num) < SIZE) {
    col = List.from(col)..add(b[y][x]);
    y = (y as num) + 1;
  }
  return col;
}

void setCol(List<List<int>> b, int x, List<int> col) {
  num y = 0;
  while ((y as num) < SIZE) {
    b[y][x] = col[y];
    y = (y as num) + 1;
  }
}

Map<String, any> moveUp(List<List<int>> b, int score) {
  var moved = false;
  num x = 0;
  while ((x as num) < SIZE) {
    var col = getCol(b, x);
    var r = slideLeft(col);
    var _new = (r as Map)['row'];
    score = score + ((r as Map)['gain'] as num);
    num y = 0;
    while ((y as num) < SIZE) {
      if (b[y][x] != _new[y]) {
        moved = true;
      }
      b[y][x] = _new[y];
      y = (y as num) + 1;
    }
    x = (x as num) + 1;
  }
  return {'board': b, 'score': score, 'moved': moved};
}

Map<String, any> moveDown(List<List<int>> b, int score) {
  var moved = false;
  num x = 0;
  while ((x as num) < SIZE) {
    var col = reverseRow(getCol(b, x));
    var r = slideLeft(col);
    col = (r as Map)['row'];
    score = score + ((r as Map)['gain'] as num);
    col = reverseRow(col);
    num y = 0;
    while ((y as num) < SIZE) {
      if (b[y][x] != col[y]) {
        moved = true;
      }
      b[y][x] = col[y];
      y = (y as num) + 1;
    }
    x = (x as num) + 1;
  }
  return {'board': b, 'score': score, 'moved': moved};
}

bool hasMoves(List<List<int>> b) {
  num y = 0;
  while ((y as num) < SIZE) {
    num x = 0;
    while ((x as num) < SIZE) {
      if (b[y][x] == 0) {
        return true;
      }
      if (((x as num) + 1 as num) < SIZE && b[y][x] == b[y][(x as num) + 1]) {
        return true;
      }
      if (((y as num) + 1 as num) < SIZE && b[y][x] == b[(y as num) + 1][x]) {
        return true;
      }
      x = (x as num) + 1;
    }
    y = (y as num) + 1;
  }
  return false;
}

bool has2048(List<List<int>> b) {
  num y = 0;
  while ((y as num) < SIZE) {
    num x = 0;
    while ((x as num) < SIZE) {
      if (b[y][x] >= 2048) {
        return true;
      }
      x = (x as num) + 1;
    }
    y = (y as num) + 1;
  }
  return false;
}

var board = newBoard();

var r = spawnTile(board);

var full = (r as Map)['full'];

num score = 0;

void main() {
  board = (r as Map)['board'];
  r = spawnTile(board);
  board = (r as Map)['board'];
  full = (r as Map)['full'];
  draw(board, score);
  while (true) {
    print('Move: ');
    var cmd = input();
    var moved = false;
    if (cmd == 'a' || cmd == 'A') {
      var m = moveLeft(board, score);
      board = m['board'];
      score = m['score'];
      moved = m['moved'];
    }
    if (cmd == 'd' || cmd == 'D') {
      var m = moveRight(board, score);
      board = m['board'];
      score = m['score'];
      moved = m['moved'];
    }
    if (cmd == 'w' || cmd == 'W') {
      var m = moveUp(board, score);
      board = m['board'];
      score = m['score'];
      moved = m['moved'];
    }
    if (cmd == 's' || cmd == 'S') {
      var m = moveDown(board, score);
      board = m['board'];
      score = m['score'];
      moved = m['moved'];
    }
    if (cmd == 'q' || cmd == 'Q') {
      break;
    }
    if (moved != null) {
      var r2 = spawnTile(board);
      board = r2['board'];
      full = r2['full'];
      if (full && (!hasMoves(board)) != null) {
        draw(board, score);
        print('Game Over');
        break;
      }
    }
    draw(board, score);
    if (has2048(board)) {
      print('You win!');
      break;
    }
    if (!hasMoves(board)) {
      print('Game Over');
      break;
    }
  }
}
