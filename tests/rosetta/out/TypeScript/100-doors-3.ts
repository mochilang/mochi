// Source: /workspace/mochi/tests/rosetta/x/Mochi/100-doors-3.mochi

var result: string;

function main(): void {
  var result: string = "";
  for (let i: number = 1; i < 101; i++) {
    var j = 1;
    while (((j * j) < i)) {
      j = j + 1;
    }
    if (((j * j) == i)) {
      result = `${result}O`;
    } else {
      result = `${result}-`;
    }
  }
  console.log(result);
}
main();
