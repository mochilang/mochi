String classify(int n) {
  switch (n) {
    case 0:
      return 'zero';
    case 1:
      return 'one';
    default:
      return 'many';
  }
}

void main() {
  var x = 2;
  var label;
  switch (x) {
    case 1:
      label = 'one';
      break;
    case 2:
      label = 'two';
      break;
    case 3:
      label = 'three';
      break;
    default:
      label = 'unknown';
  }
  print(label);

  var day = 'sun';
  var mood;
  switch (day) {
    case 'mon':
      mood = 'tired';
      break;
    case 'fri':
      mood = 'excited';
      break;
    case 'sun':
      mood = 'relaxed';
      break;
    default:
      mood = 'normal';
  }
  print(mood);

  var ok = true;
  var status;
  switch (ok) {
    case true:
      status = 'confirmed';
      break;
    case false:
      status = 'denied';
      break;
  }
  print(status);

  print(classify(0));
  print(classify(5));
}
