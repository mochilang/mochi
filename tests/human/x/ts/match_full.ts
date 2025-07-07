const x = 2;
let label: string;
switch (x) {
  case 1:
    label = "one";
    break;
  case 2:
    label = "two";
    break;
  case 3:
    label = "three";
    break;
  default:
    label = "unknown";
}
console.log(label);

const day = "sun";
let mood: string;
switch (day) {
  case "mon":
    mood = "tired";
    break;
  case "fri":
    mood = "excited";
    break;
  case "sun":
    mood = "relaxed";
    break;
  default:
    mood = "normal";
}
console.log(mood);

const ok = true;
const status = ok ? "confirmed" : "denied";
console.log(status);

function classify(n: number): string {
  switch (n) {
    case 0:
      return "zero";
    case 1:
      return "one";
    default:
      return "many";
  }
}
console.log(classify(0));
console.log(classify(5));
