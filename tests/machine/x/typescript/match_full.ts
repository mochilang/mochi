const x = 2;
const label = (() => {
  const _tmp51 = x;
  let _res;
  switch (_tmp51) {
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
const day = "sun";
const mood = (() => {
  const _tmp52 = day;
  let _res;
  switch (_tmp52) {
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
const ok = true;
const status = (() => {
  const _tmp53 = ok;
  let _res;
  switch (_tmp53) {
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
  const _tmp54 = n;
  let _res;
  switch (_tmp54) {
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
