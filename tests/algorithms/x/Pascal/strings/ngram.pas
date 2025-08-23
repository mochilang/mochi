{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  example1: string;
  example2: string;
  example3: string;
function create_ngram(create_ngram_sentence: string; create_ngram_ngram_size: integer): StrArray; forward;
function create_ngram(create_ngram_sentence: string; create_ngram_ngram_size: integer): StrArray;
var
  create_ngram_res: array of string;
  create_ngram_bound: integer;
  create_ngram_i: integer;
begin
  create_ngram_res := [];
  create_ngram_bound := (Length(create_ngram_sentence) - create_ngram_ngram_size) + 1;
  if create_ngram_bound <= 0 then begin
  exit(create_ngram_res);
end;
  create_ngram_i := 0;
  while create_ngram_i < create_ngram_bound do begin
  create_ngram_res := concat(create_ngram_res, StrArray([copy(create_ngram_sentence, create_ngram_i+1, (create_ngram_i + create_ngram_ngram_size - (create_ngram_i)))]));
  create_ngram_i := create_ngram_i + 1;
end;
  exit(create_ngram_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example1 := 'I am a sentence';
  writeln(list_to_str(create_ngram(example1, 2)));
  example2 := 'I am an NLPer';
  writeln(list_to_str(create_ngram(example2, 2)));
  example3 := 'This is short';
  writeln(list_to_str(create_ngram(example3, 50)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
