// Generated by Mochi TypeScript compiler

function fractionToDecimal(numerator: number, denominator: number): string {
  if ((denominator == 0)) {
    return "";
  }
  if ((numerator == 0)) {
    return "0";
  }
  let result: string = "";
  (globalThis as any).result = result;
  let minus: string = _indexString(String(-1), 0);
  (globalThis as any).minus = minus;
  let negative: boolean = false;
  (globalThis as any).negative = negative;
  if (
    (((numerator < 0) && (denominator > 0)) ||
      ((numerator > 0) && (denominator < 0)))
  ) {
    negative = true;
  }
  if ((numerator < 0)) {
    numerator = -numerator;
  }
  if ((denominator < 0)) {
    denominator = -denominator;
  }
  let integerPart: number = Math.trunc(numerator / denominator);
  (globalThis as any).integerPart = integerPart;
  result = String(integerPart);
  let remainder: number = numerator % denominator;
  (globalThis as any).remainder = remainder;
  if ((remainder == 0)) {
    if (negative) {
      return minus + result;
    }
    return result;
  }
  result = result + ".";
  let mapIndex: Record<number, number> = {};
  (globalThis as any).mapIndex = mapIndex;
  let decimal: string = "";
  (globalThis as any).decimal = decimal;
  while ((remainder != 0)) {
    if (Object.prototype.hasOwnProperty.call(mapIndex, String(remainder))) {
      let idx: number = mapIndex[remainder];
      (globalThis as any).idx = idx;
      let prefix: string = _sliceString(decimal, 0, idx);
      (globalThis as any).prefix = prefix;
      let suffix: string = _sliceString(decimal, idx, decimal.length);
      (globalThis as any).suffix = suffix;
      decimal = prefix + "(" + suffix + ")";
      break;
    }
    mapIndex[remainder] = decimal.length;
    remainder = remainder * 10;
    let digit: number = Math.trunc(remainder / denominator);
    (globalThis as any).digit = digit;
    decimal = decimal + String(digit);
    remainder = remainder % denominator;
  }
  result = result + decimal;
  if (negative) {
    return minus + result;
  }
  return result;
}

function test_example_1(): void {
  if (!(fractionToDecimal(1, 2) == "0.5")) throw new Error("expect failed");
}

function test_example_2(): void {
  if (!(fractionToDecimal(2, 1) == "2")) throw new Error("expect failed");
}

function test_example_3(): void {
  if (!(fractionToDecimal(2, 3) == "0.(6)")) throw new Error("expect failed");
}

function test_negative(): void {
  if (!(fractionToDecimal(-50, 8) == "-6.25")) throw new Error("expect failed");
}

function test_repeat_zeros(): void {
  if (!(fractionToDecimal(1, 6) == "0.1(6)")) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
  test_negative();
  test_repeat_zeros();
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

function _sliceString(s: string, i: number, j: number): string {
  let start = i;
  let end = j;
  const runes = Array.from(s);
  const n = runes.length;
  if (start < 0) start += n;
  if (end < 0) end += n;
  if (start < 0) start = 0;
  if (end > n) end = n;
  if (end < start) end = start;
  return runes.slice(start, end).join("");
}

main();
