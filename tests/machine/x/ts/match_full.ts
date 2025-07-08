let x = 2;
let label = (() => {
  const _tmp1 = x;
  let _res;
  switch (_tmp1) {
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
let day = "sun";
let mood = (() => {
  const _tmp2 = day;
  let _res;
  switch (_tmp2) {
    case "mon":
      _res = "tired";
      break;
    case "fri":
      _res = "excited";
      break;
    case "sun":
      _res = "relaxed";
      break;
    default:
      _res = "normal";
      break;
  }
  return _res;
})()
;
console.log(mood);
let ok = true;
let status = (() => {
  const _tmp3 = ok;
  let _res;
  switch (_tmp3) {
    case true:
      _res = "confirmed";
      break;
    case false:
      _res = "denied";
      break;
    default:
      _res = undefined;
      break;
  }
  return _res;
})()
;
console.log(status);
function classify(n) {
  return (() => {
  const _tmp4 = n;
  let _res;
  switch (_tmp4) {
    case 0:
      _res = "zero";
      break;
    case 1:
      _res = "one";
      break;
    default:
      _res = "many";
      break;
  }
  return _res;
})()
;
}
console.log(classify(0));
console.log(classify(5));
