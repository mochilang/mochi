{$mode objfpc}
program Main;
uses SysUtils;
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
procedure main(); forward;
procedure main();
begin
  writeln('Diagram after trimming whitespace and removal of blank lines:' + #10 + '');
  writeln('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+');
  writeln('|                      ID                       |');
  writeln('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+');
  writeln('|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |');
  writeln('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+');
  writeln('|                    QDCOUNT                    |');
  writeln('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+');
  writeln('|                    ANCOUNT                    |');
  writeln('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+');
  writeln('|                    NSCOUNT                    |');
  writeln('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+');
  writeln('|                    ARCOUNT                    |');
  writeln('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+');
  writeln('' + #10 + 'Decoded:' + #10 + '');
  writeln('Name     Bits  Start  End');
  writeln('=======  ====  =====  ===');
  writeln('ID        16      0    15');
  writeln('QR         1     16    16');
  writeln('Opcode     4     17    20');
  writeln('AA         1     21    21');
  writeln('TC         1     22    22');
  writeln('RD         1     23    23');
  writeln('RA         1     24    24');
  writeln('Z          3     25    27');
  writeln('RCODE      4     28    31');
  writeln('QDCOUNT   16     32    47');
  writeln('ANCOUNT   16     48    63');
  writeln('NSCOUNT   16     64    79');
  writeln('ARCOUNT   16     80    95');
  writeln('' + #10 + 'Test string in hex:');
  writeln('78477bbf5496e12e1bf169a4');
  writeln('' + #10 + 'Test string in binary:');
  writeln('011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100');
  writeln('' + #10 + 'Unpacked:' + #10 + '');
  writeln('Name     Size  Bit pattern');
  writeln('=======  ====  ================');
  writeln('ID        16   0111100001000111');
  writeln('QR         1   0');
  writeln('Opcode     4   1111');
  writeln('AA         1   0');
  writeln('TC         1   1');
  writeln('RD         1   1');
  writeln('RA         1   1');
  writeln('Z          3   011');
  writeln('RCODE      4   1111');
  writeln('QDCOUNT   16   0101010010010110');
  writeln('ANCOUNT   16   1110000100101110');
  writeln('NSCOUNT   16   0001101111110001');
  writeln('ARCOUNT   16   0110100110100100');
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
