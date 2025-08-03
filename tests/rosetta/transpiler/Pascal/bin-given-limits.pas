{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
  bins: IntArray;
function getBins(limits: IntArray; data: IntArray): IntArray; forward;
function padLeft(n: integer; width: integer): string; forward;
procedure printBins(limits: IntArray; bins: IntArray); forward;
procedure main(); forward;
function getBins(limits: IntArray; data: IntArray): IntArray;
var
  getBins_n: integer;
  getBins_bins: array of integer;
  getBins_i: integer;
  getBins_j: integer;
  getBins_d: integer;
  getBins_index: integer;
begin
  getBins_n := Length(limits);
  getBins_bins := [];
  getBins_i := 0;
  while getBins_i < (getBins_n + 1) do begin
  getBins_bins := concat(getBins_bins, [0]);
  getBins_i := getBins_i + 1;
end;
  getBins_j := 0;
  while getBins_j < Length(data) do begin
  getBins_d := data[getBins_j];
  getBins_index := 0;
  while getBins_index < Length(limits) do begin
  if getBins_d < limits[getBins_index] then begin
  break;
end;
  if getBins_d = limits[getBins_index] then begin
  getBins_index := getBins_index + 1;
  break;
end;
  getBins_index := getBins_index + 1;
end;
  getBins_bins[getBins_index] := getBins_bins[getBins_index] + 1;
  getBins_j := getBins_j + 1;
end;
  exit(getBins_bins);
end;
function padLeft(n: integer; width: integer): string;
var
  padLeft_s: string;
  padLeft_pad: integer;
  padLeft_out: string;
  padLeft_i: integer;
begin
  padLeft_s := IntToStr(n);
  padLeft_pad := width - Length(padLeft_s);
  padLeft_out := '';
  padLeft_i := 0;
  while padLeft_i < padLeft_pad do begin
  padLeft_out := padLeft_out + ' ';
  padLeft_i := padLeft_i + 1;
end;
  exit(padLeft_out + padLeft_s);
end;
procedure printBins(limits: IntArray; bins: IntArray);
var
  printBins_i: integer;
begin
  n := Length(limits);
  writeln((('           < ' + padLeft(limits[0], 3)) + ' = ') + padLeft(bins[0], 2));
  printBins_i := 1;
  while printBins_i < n do begin
  writeln((((('>= ' + padLeft(limits[printBins_i - 1], 3)) + ' and < ') + padLeft(limits[printBins_i], 3)) + ' = ') + padLeft(bins[printBins_i], 2));
  printBins_i := printBins_i + 1;
end;
  writeln((('>= ' + padLeft(limits[n - 1], 3)) + '           = ') + padLeft(bins[n], 2));
  writeln('');
end;
procedure main();
var
  main_limitsList: array of array of integer;
  main_dataList: array of array of integer;
  main_i: integer;
begin
  main_limitsList := [[23, 37, 43, 53, 67, 83], [14, 18, 249, 312, 389, 392, 513, 591, 634, 720]];
  main_dataList := [[95, 21, 94, 12, 99, 4, 70, 75, 83, 93, 52, 80, 57, 5, 53, 86, 65, 17, 92, 83, 71, 61, 54, 58, 47, 16, 8, 9, 32, 84, 7, 87, 46, 19, 30, 37, 96, 6, 98, 40, 79, 97, 45, 64, 60, 29, 49, 36, 43, 55], [445, 814, 519, 697, 700, 130, 255, 889, 481, 122, 932, 77, 323, 525, 570, 219, 367, 523, 442, 933, 416, 589, 930, 373, 202, 253, 775, 47, 731, 685, 293, 126, 133, 450, 545, 100, 741, 583, 763, 306, 655, 267, 248, 477, 549, 238, 62, 678, 98, 534, 622, 907, 406, 714, 184, 391, 913, 42, 560, 247, 346, 860, 56, 138, 546, 38, 985, 948, 58, 213, 799, 319, 390, 634, 458, 945, 733, 507, 916, 123, 345, 110, 720, 917, 313, 845, 426, 9, 457, 628, 410, 723, 354, 895, 881, 953, 677, 137, 397, 97, 854, 740, 83, 216, 421, 94, 517, 479, 292, 963, 376, 981, 480, 39, 257, 272, 157, 5, 316, 395, 787, 942, 456, 242, 759, 898, 576, 67, 298, 425, 894, 435, 831, 241, 989, 614, 987, 770, 384, 692, 698, 765, 331, 487, 251, 600, 879, 342, 982, 527, 736, 795, 585, 40, 54, 901, 408, 359, 577, 237, 605, 847, 353, 968, 832, 205, 838, 427, 876, 959, 686, 646, 835, 127, 621, 892, 443, 198, 988, 791, 466, 23, 707, 467, 33, 670, 921, 180, 991, 396, 160, 436, 717, 918, 8, 374, 101, 684, 727, 749]];
  main_i := 0;
  while main_i < Length(main_limitsList) do begin
  writeln(('Example ' + IntToStr(main_i + 1)) + '' + #10 + '');
  bins := getBins(main_limitsList[main_i], main_dataList[main_i]);
  printBins(main_limitsList[main_i], bins);
  main_i := main_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
