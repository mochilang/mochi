var x = 2;

var label = (() {
  var _t = x;
  if (_t == 1) {
    return 'one';
  } else if (_t == 2) {
    return 'two';
  } else if (_t == 3) {
    return 'three';
  } else {
    return 'unknown';
  }
  return null;
})();

void main() {
  print(label);
}
