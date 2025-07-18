// Source: /workspace/mochi/tests/rosetta/x/Mochi/100-prisoners.mochi

function shuffle(xs: number[]): number[] {
  var arr = xs;
  var i = 99;
  while ((i > 0)) {
    let j = performance.now() * 1000000 % (i + 1);
    let tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
    i = i - 1;
  }
  return arr;
}

function doTrials(trials: number, np: number, strategy: string): void {
  var pardoned = 0;
  var t = 0;
  while ((t < trials)) {
    var drawers: number[] = [];
    var i = 0;
    while ((i < 100)) {
      drawers = [...drawers, i];
      i = i + 1;
    }
    drawers = shuffle(drawers);
    var p = 0;
    var success = true;
    while ((p < np)) {
      var found = false;
      if ((strategy == "optimal")) {
        var prev = p;
        var d = 0;
        while ((d < 50)) {
          let _this = drawers[prev];
          if ((_this == p)) {
            found = true;
            break;
          }
          prev = _this;
          d = d + 1;
        }
      } else {
        var opened: boolean[] = [];
        var k = 0;
        while ((k < 100)) {
          opened = [...opened, false];
          k = k + 1;
        }
        var d = 0;
        while ((d < 50)) {
          var n = performance.now() * 1000000 % 100;
          while (opened[n]) {
            n = performance.now() * 1000000 % 100;
          }
          opened[n] = true;
          if ((drawers[n] == p)) {
            found = true;
            break;
          }
          d = d + 1;
        }
      }
      if ((!found)) {
        success = false;
        break;
      }
      p = p + 1;
    }
    if (success) {
      pardoned = pardoned + 1;
    }
    t = t + 1;
  }
  let rf = ((pardoned as number) / (trials as number)) * 100;
  return console.log(
    `  strategy = ${strategy}  pardoned = ${
      String(pardoned)
    } relative frequency = ${String(rf)}%`,
  );
}

function main(): void {
  let trials = 1000;
  for (const np of [10, 100]) {
    console.log(
      `Results from ${String(trials)} trials with ${String(np)} prisoners:
`,
    );
    for (const strat of ["random", "optimal"]) {
      doTrials(trials, np, strat);
    }
  }
}

main();
