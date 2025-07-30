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
type Parser = record
  expr: string;
  pos: integer;
end;
type Res = record
  v: integer;
  p: Parser;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeRes(v: integer; p: Parser): Res; forward;
function makeParser(expr: string; pos: integer): Parser; forward;
function skipWS(p: Parser): Parser; forward;
function parseIntStr(str: string): integer; forward;
function parseNumber(p: Parser): Res; forward;
function parseFactor(p: Parser): Res; forward;
function powInt(base: integer; exp: integer): integer; forward;
function parsePower(p: Parser): Res; forward;
function parseTerm(p: Parser): Res; forward;
function parseExpr(p: Parser): Res; forward;
function evalExpr(expr: string): integer; forward;
procedure main(); forward;
function makeRes(v: integer; p: Parser): Res;
begin
  Result.v := v;
  Result.p := p;
end;
function makeParser(expr: string; pos: integer): Parser;
begin
  Result.expr := expr;
  Result.pos := pos;
end;
function skipWS(p: Parser): Parser;
var
  skipWS_i: integer;
begin
  skipWS_i := p.pos;
  while (skipWS_i < Length(p.expr)) and (copy(p.expr, skipWS_i+1, (skipWS_i + 1 - (skipWS_i))) = ' ') do begin
  skipWS_i := skipWS_i + 1;
end;
  p.pos := skipWS_i;
  exit(p);
end;
function parseIntStr(str: string): integer;
var
  parseIntStr_i: integer;
  parseIntStr_n: integer;
begin
  exit(StrToInt(str));
end;
function parseNumber(p: Parser): Res;
var
  parseNumber_start: integer;
  parseNumber_ch: string;
  parseNumber_token: string;
begin
  p := skipWS(p);
  parseNumber_start := p.pos;
  while p.pos < Length(p.expr) do begin
  parseNumber_ch := copy(p.expr, p.pos+1, (p.pos + 1 - (p.pos)));
  if (parseNumber_ch >= '0') and (parseNumber_ch <= '9') then begin
  p.pos := p.pos + 1;
end else begin
  break;
end;
end;
  parseNumber_token := copy(p.expr, parseNumber_start+1, (p.pos - (parseNumber_start)));
  exit(makeRes(parseIntStr(parseNumber_token), p));
end;
function parseFactor(p: Parser): Res;
var
  parseFactor_r: integer;
  parseFactor_v: integer;
begin
  p := skipWS(p);
  if (p.pos < Length(p.expr)) and (copy(p.expr, p.pos+1, (p.pos + 1 - (p.pos))) = '(') then begin
  p.pos := p.pos + 1;
  parseFactor_r := parseExpr(p);
  parseFactor_v := parseFactor_r.v;
  p := parseFactor_r.p;
  p := skipWS(p);
  if (p.pos < Length(p.expr)) and (copy(p.expr, p.pos+1, (p.pos + 1 - (p.pos))) = ')') then begin
  p.pos := p.pos + 1;
end;
  exit(makeRes(parseFactor_v, p));
end;
  if (p.pos < Length(p.expr)) and (copy(p.expr, p.pos+1, (p.pos + 1 - (p.pos))) = '-') then begin
  p.pos := p.pos + 1;
  parseFactor_r := parseFactor(p);
  parseFactor_v := parseFactor_r.v;
  p := parseFactor_r.p;
  exit(makeRes(-parseFactor_v, p));
end;
  exit(parseNumber(p));
end;
function powInt(base: integer; exp: integer): integer;
var
  powInt_r: integer;
  powInt_b: integer;
  powInt_e: integer;
begin
  powInt_r := 1;
  powInt_b := base;
  powInt_e := exp;
  while powInt_e > 0 do begin
  if (powInt_e mod 2) = 1 then begin
  powInt_r := powInt_r * powInt_b;
end;
  powInt_b := powInt_b * powInt_b;
  powInt_e := powInt_e div Trunc(2);
end;
  exit(powInt_r);
end;
function parsePower(p: Parser): Res;
var
  parsePower_r: Res;
  parsePower_v: integer;
  parsePower_r2: Res;
  parsePower_rhs: integer;
begin
  parsePower_r := parseFactor(p);
  parsePower_v := parsePower_r.v;
  p := parsePower_r.p;
  while true do begin
  p := skipWS(p);
  if (p.pos < Length(p.expr)) and (copy(p.expr, p.pos+1, (p.pos + 1 - (p.pos))) = '^') then begin
  p.pos := p.pos + 1;
  parsePower_r2 := parseFactor(p);
  parsePower_rhs := parsePower_r2.v;
  p := parsePower_r2.p;
  parsePower_v := powInt(parsePower_v, parsePower_rhs);
end else begin
  break;
end;
end;
  exit(makeRes(parsePower_v, p));
end;
function parseTerm(p: Parser): Res;
var
  parseTerm_r: Res;
  parseTerm_v: integer;
  parseTerm_op: string;
  parseTerm_r2: Res;
  parseTerm_rhs: integer;
begin
  parseTerm_r := parsePower(p);
  parseTerm_v := parseTerm_r.v;
  p := parseTerm_r.p;
  while true do begin
  p := skipWS(p);
  if p.pos < Length(p.expr) then begin
  parseTerm_op := copy(p.expr, p.pos+1, (p.pos + 1 - (p.pos)));
  if parseTerm_op = '*' then begin
  p.pos := p.pos + 1;
  parseTerm_r2 := parsePower(p);
  parseTerm_rhs := parseTerm_r2.v;
  p := parseTerm_r2.p;
  parseTerm_v := parseTerm_v * parseTerm_rhs;
  continue;
end;
  if parseTerm_op = '/' then begin
  p.pos := p.pos + 1;
  parseTerm_r2 := parsePower(p);
  parseTerm_rhs := parseTerm_r2.v;
  p := parseTerm_r2.p;
  parseTerm_v := parseTerm_v div Trunc(parseTerm_rhs);
  continue;
end;
end;
  break;
end;
  exit(makeRes(parseTerm_v, p));
end;
function parseExpr(p: Parser): Res;
var
  parseExpr_r: Res;
  parseExpr_v: integer;
  parseExpr_op: string;
  parseExpr_r2: Res;
  parseExpr_rhs: integer;
begin
  parseExpr_r := parseTerm(p);
  parseExpr_v := parseExpr_r.v;
  p := parseExpr_r.p;
  while true do begin
  p := skipWS(p);
  if p.pos < Length(p.expr) then begin
  parseExpr_op := copy(p.expr, p.pos+1, (p.pos + 1 - (p.pos)));
  if parseExpr_op = '+' then begin
  p.pos := p.pos + 1;
  parseExpr_r2 := parseTerm(p);
  parseExpr_rhs := parseExpr_r2.v;
  p := parseExpr_r2.p;
  parseExpr_v := parseExpr_v + parseExpr_rhs;
  continue;
end;
  if parseExpr_op = '-' then begin
  p.pos := p.pos + 1;
  parseExpr_r2 := parseTerm(p);
  parseExpr_rhs := parseExpr_r2.v;
  p := parseExpr_r2.p;
  parseExpr_v := parseExpr_v - parseExpr_rhs;
  continue;
end;
end;
  break;
end;
  exit(makeRes(parseExpr_v, p));
end;
function evalExpr(expr: string): integer;
var
  evalExpr_p: Parser;
  evalExpr_r: Res;
begin
  evalExpr_p := makeParser(expr, 0);
  evalExpr_r := parseExpr(evalExpr_p);
  exit(evalExpr_r.v);
end;
procedure main();
var
  main_expr: string;
begin
  main_expr := '2*(3-1)+2*5';
  writeln((main_expr + ' = ') + IntToStr(evalExpr(main_expr)));
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
