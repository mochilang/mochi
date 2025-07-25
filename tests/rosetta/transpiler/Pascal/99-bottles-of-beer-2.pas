{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
function _sys_now(): integer;
begin
  _sys_now := Integer(GetTickCount64()*1000);
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := _sys_now();
  end;
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
  fields_words: array of string;
  fields_cur: string;
  fields_i: integer;
  fields_ch: string;
  join_res: string;
  join_i: integer;
  numberName_small: array of string;
  numberName_tens: array of string;
  numberName_t: string;
  numberName_s: integer;
  pluralizeFirst_w: StrArray;
  randInt_next: integer;
  slur_a: array of string;
  slur_i: integer;
  slur_idx: integer;
  slur_seed: integer;
  slur_j: integer;
  slur_tmp: string;
  slur_s: string;
  slur_k: integer;
  slur_w: StrArray;
  main_i: integer;
function fields(s: string): StrArray;
begin
  fields_words := [];
  fields_cur := '';
  fields_i := 0;
  while fields_i < Length(s) do begin
  fields_ch := copy(s, fields_i+1, (fields_i + 1 - fields_i));
  if ((fields_ch = ' ') or (fields_ch = '' + #10 + '')) or (fields_ch = '	') then begin
  if Length(fields_cur) > 0 then begin
  fields_words := concat(fields_words, [fields_cur]);
  fields_cur := '';
end;
end else begin
  fields_cur := fields_cur + fields_ch;
end;
  fields_i := fields_i + 1;
end;
  if Length(fields_cur) > 0 then begin
  fields_words := concat(fields_words, [fields_cur]);
end;
  exit(fields_words);
end;
function join(xs: StrArray; sep: string): string;
begin
  join_res := '';
  join_i := 0;
  while join_i < Length(xs) do begin
  if join_i > 0 then begin
  join_res := join_res + sep;
end;
  join_res := join_res + xs[join_i];
  join_i := join_i + 1;
end;
  exit(join_res);
end;
function numberName(n: integer): string;
begin
  numberName_small := ['no', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
  numberName_tens := ['ones', 'ten', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
  if n < 0 then begin
  exit('');
end;
  if n < 20 then begin
  exit(numberName_small[n]);
end;
  if n < 100 then begin
  numberName_t := numberName_tens[Trunc(n div 10)];
  numberName_s := n mod 10;
  if numberName_s > 0 then begin
  numberName_t := (numberName_t + ' ') + numberName_small[numberName_s];
end;
  exit(numberName_t);
end;
  exit('');
end;
function pluralizeFirst(s: string; n: integer): string;
begin
  if n = 1 then begin
  exit(s);
end;
  pluralizeFirst_w := fields(s);
  if Length(pluralizeFirst_w) > 0 then begin
  pluralizeFirst_w[0] := pluralizeFirst_w[0] + 's';
end;
  exit(join(pluralizeFirst_w, ' '));
end;
function randInt(seed: integer; n: integer): integer;
begin
  randInt_next := ((seed * 1664525) + 1013904223) mod 2147483647;
  exit(randInt_next mod n);
end;
function slur(p: string; d: integer): string;
begin
  if Length(p) <= 2 then begin
  exit(p);
end;
  slur_a := [];
  slur_i := 1;
  while slur_i < (Length(p) - 1) do begin
  slur_a := concat(slur_a, [copy(p, slur_i+1, (slur_i + 1 - slur_i))]);
  slur_i := slur_i + 1;
end;
  slur_idx := Length(slur_a) - 1;
  slur_seed := d;
  while slur_idx >= 1 do begin
  slur_seed := ((slur_seed * 1664525) + 1013904223) mod 2147483647;
  if (slur_seed mod 100) >= d then begin
  slur_j := slur_seed mod (slur_idx + 1);
  slur_tmp := slur_a[slur_idx];
  slur_a[slur_idx] := slur_a[slur_j];
  slur_a[slur_j] := slur_tmp;
end;
  slur_idx := slur_idx - 1;
end;
  slur_s := copy(p, 0+1, (1 - 0));
  slur_k := 0;
  while slur_k < Length(slur_a) do begin
  slur_s := slur_s + slur_a[slur_k];
  slur_k := slur_k + 1;
end;
  slur_s := slur_s + copy(p, Length(p) - 1+1, (Length(p) - Length(p) - 1));
  slur_w := fields(slur_s);
  exit(join(slur_w, ' '));
end;
procedure main();
begin
  main_i := 99;
  while main_i > 0 do begin
  writeln((((slur(numberName(main_i), main_i) + ' ') + pluralizeFirst(slur('bottle of', main_i), main_i)) + ' ') + slur('beer on the wall', main_i));
  writeln((((slur(numberName(main_i), main_i) + ' ') + pluralizeFirst(slur('bottle of', main_i), main_i)) + ' ') + slur('beer', main_i));
  writeln((((slur('take one', main_i) + ' ') + slur('down', main_i)) + ' ') + slur('pass it around', main_i));
  writeln((((slur(numberName(main_i - 1), main_i) + ' ') + pluralizeFirst(slur('bottle of', main_i), main_i - 1)) + ' ') + slur('beer on the wall', main_i));
  main_i := main_i - 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _sys_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_sys_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
