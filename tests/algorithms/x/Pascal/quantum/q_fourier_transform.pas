{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
function map_str_int_to_str(m: specialize TFPGMap<string, integer>): string;
var i: integer;
begin
  Result := 'map[';
  for i := 0 to m.Count - 1 do begin
    Result := Result + m.Keys[i];
    Result := Result + ':';
    Result := Result + IntToStr(m.Data[i]);
    if i < m.Count - 1 then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
  width: integer;
  number_of_qubits: integer;
function to_bits(n: integer; width: integer): string; forward;
function quantum_fourier_transform(number_of_qubits: integer): specialize TFPGMap<string, integer>; forward;
function to_bits(n: integer; width: integer): string;
var
  to_bits_res: string;
  to_bits_num: integer;
  to_bits_w: integer;
begin
  to_bits_res := '';
  to_bits_num := n;
  to_bits_w := width;
  while to_bits_w > 0 do begin
  to_bits_res := IntToStr(to_bits_num mod 2) + to_bits_res;
  to_bits_num := to_bits_num div 2;
  to_bits_w := to_bits_w - 1;
end;
  exit(to_bits_res);
end;
function quantum_fourier_transform(number_of_qubits: integer): specialize TFPGMap<string, integer>;
var
  quantum_fourier_transform_shots: integer;
  quantum_fourier_transform_states: integer;
  quantum_fourier_transform_p: integer;
  quantum_fourier_transform_per_state: integer;
  quantum_fourier_transform_counts: specialize TFPGMap<string, integer>;
  quantum_fourier_transform_i: integer;
begin
  if number_of_qubits <= 0 then begin
  panic('number of qubits must be > 0.');
end;
  if number_of_qubits > 10 then begin
  panic('number of qubits too large to simulate(>10).');
end;
  quantum_fourier_transform_shots := 10000;
  quantum_fourier_transform_states := 1;
  quantum_fourier_transform_p := 0;
  while quantum_fourier_transform_p < number_of_qubits do begin
  quantum_fourier_transform_states := quantum_fourier_transform_states * 2;
  quantum_fourier_transform_p := quantum_fourier_transform_p + 1;
end;
  quantum_fourier_transform_per_state := quantum_fourier_transform_shots div quantum_fourier_transform_states;
  quantum_fourier_transform_counts := specialize TFPGMap<string, integer>.Create();
  quantum_fourier_transform_i := 0;
  while quantum_fourier_transform_i < quantum_fourier_transform_states do begin
  quantum_fourier_transform_counts[to_bits(quantum_fourier_transform_i, number_of_qubits)] := quantum_fourier_transform_per_state;
  quantum_fourier_transform_i := quantum_fourier_transform_i + 1;
end;
  exit(quantum_fourier_transform_counts);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('Total count for quantum fourier transform state is: ' + map_str_int_to_str(quantum_fourier_transform(3)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
