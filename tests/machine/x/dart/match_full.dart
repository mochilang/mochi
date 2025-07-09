var x = 2;

var label = (() {
  var _t = x;
  if (_t == 1) {
    return 'one';
  } else if (_t == 2) {
    return 'two';
  } else if (_t == 3) {
    return 'three';
  }  else {
    return 'unknown';
  }
  return null;
})();

var day = 'sun';

var mood = (() {
  var _t = day;
  if (_t == 'mon') {
    return 'tired';
  } else if (_t == 'fri') {
    return 'excited';
  } else if (_t == 'sun') {
    return 'relaxed';
  }  else {
    return 'normal';
  }
  return null;
})();

var ok = true;

var status = (() {
  var _t = ok;
  if (_t == true) {
    return 'confirmed';
  } else if (_t == false) {
    return 'denied';
  }  return null;
})();

String classify(int n) {
  return (() {
  var _t = n;
  if (_t == 0) {
    return 'zero';
  } else if (_t == 1) {
    return 'one';
  }  else {
    return 'many';
  }
  return null;
})();
}

void main() {
  print(label);
  print(mood);
  print(status);
  print(classify(0));
  print(classify(5));
}
