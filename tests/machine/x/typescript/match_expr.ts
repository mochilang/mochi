const x = 2;
const label = (() => {
  const _tmp50 = x;
  let _res;
  switch (_tmp50) {
    case 1:
      _res = "one";
      break;
    case 2:
      _res = "two";
      break;
    case 3:
      _res = "three";
      break;
    default:
      _res = "unknown";
      break;
  }
  return _res;
})()
;
console.log(label);
