// Generated by Mochi compiler v0.10.30 on 2025-07-19T02:08:26Z
// Source: /workspace/mochi/tests/rosetta/x/Mochi/15-puzzle-solver.mochi

const testpkg = {
  Add: (a: number, b: number) => a + b,
  Pi: 3.14,
  Answer: 42,
  FifteenPuzzleExample: () =>
    "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd",
};

function main(): void {
  console.log(testpkg.FifteenPuzzleExample());
}
main();
