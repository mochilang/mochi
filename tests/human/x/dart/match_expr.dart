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
}
