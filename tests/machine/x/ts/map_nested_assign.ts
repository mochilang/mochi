// Generated by Mochi TypeScript compiler

var data: { [key: string]: { [key: string]: number } };

function main(): void {
  data = { "outer": { "inner": 1 } };
  data["outer"]["inner"] = 2;
  console.log(data["outer"]["inner"]);
}
main();
