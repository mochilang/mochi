{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type BoolArray = array of boolean;
type IntArrayArray = array of IntArray;
type State = record
  claim: array of integer;
  alloc: array of IntArray;
  max: array of IntArray;
end;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  claim_vector: array of integer;
  allocated_resources_table: array of IntArray;
  maximum_claim_table: array of IntArray;
  max: IntArrayArray;
  claim: IntArray;
  alloc: IntArrayArray;
  alloc_sum: IntArray;
function makeState(claim: IntArray; alloc: IntArrayArray; max: IntArrayArray): State; forward;
function processes_resource_summation(alloc: IntArrayArray): IntArray; forward;
function available_resources(claim: IntArray; alloc_sum: IntArray): IntArray; forward;
function need(max: IntArrayArray; alloc: IntArrayArray): IntArrayArray; forward;
procedure pretty_print(claim: IntArray; alloc: IntArrayArray; max: IntArrayArray); forward;
procedure bankers_algorithm(claim: IntArray; alloc: IntArrayArray; max: IntArrayArray); forward;
function makeState(claim: IntArray; alloc: IntArrayArray; max: IntArrayArray): State;
begin
  Result.claim := claim;
  Result.alloc := alloc;
  Result.max := max;
end;
function processes_resource_summation(alloc: IntArrayArray): IntArray;
var
  processes_resource_summation_resources: integer;
  processes_resource_summation_sums: array of integer;
  processes_resource_summation_i: integer;
  processes_resource_summation_total: integer;
  processes_resource_summation_j: integer;
begin
  processes_resource_summation_resources := Length(alloc[0]);
  processes_resource_summation_sums := [];
  processes_resource_summation_i := 0;
  while processes_resource_summation_i < processes_resource_summation_resources do begin
  processes_resource_summation_total := 0;
  processes_resource_summation_j := 0;
  while processes_resource_summation_j < Length(alloc) do begin
  processes_resource_summation_total := processes_resource_summation_total + alloc[processes_resource_summation_j][processes_resource_summation_i];
  processes_resource_summation_j := processes_resource_summation_j + 1;
end;
  processes_resource_summation_sums := concat(processes_resource_summation_sums, IntArray([processes_resource_summation_total]));
  processes_resource_summation_i := processes_resource_summation_i + 1;
end;
  exit(processes_resource_summation_sums);
end;
function available_resources(claim: IntArray; alloc_sum: IntArray): IntArray;
var
  available_resources_avail: array of integer;
  available_resources_i: integer;
begin
  available_resources_avail := [];
  available_resources_i := 0;
  while available_resources_i < Length(claim) do begin
  available_resources_avail := concat(available_resources_avail, IntArray([claim[available_resources_i] - alloc_sum[available_resources_i]]));
  available_resources_i := available_resources_i + 1;
end;
  exit(available_resources_avail);
end;
function need(max: IntArrayArray; alloc: IntArrayArray): IntArrayArray;
var
  need_needs: array of IntArray;
  need_i: integer;
  need_row: array of integer;
  need_j: integer;
begin
  need_needs := [];
  need_i := 0;
  while need_i < Length(max) do begin
  need_row := [];
  need_j := 0;
  while need_j < Length(max[0]) do begin
  need_row := concat(need_row, IntArray([max[need_i][need_j] - alloc[need_i][need_j]]));
  need_j := need_j + 1;
end;
  need_needs := concat(need_needs, [need_row]);
  need_i := need_i + 1;
end;
  exit(need_needs);
end;
procedure pretty_print(claim: IntArray; alloc: IntArrayArray; max: IntArrayArray);
var
  pretty_print_i: integer;
  pretty_print_row: array of integer;
  pretty_print_line: string;
  pretty_print_j: integer;
  pretty_print_usage: string;
  pretty_print_alloc_sum: IntArray;
  pretty_print_avail: IntArray;
  pretty_print_avail_str: string;
begin
  writeln('         Allocated Resource Table');
  pretty_print_i := 0;
  while pretty_print_i < Length(alloc) do begin
  pretty_print_row := alloc[pretty_print_i];
  pretty_print_line := ('P' + IntToStr(pretty_print_i + 1)) + '       ';
  pretty_print_j := 0;
  while pretty_print_j < Length(pretty_print_row) do begin
  pretty_print_line := pretty_print_line + IntToStr(pretty_print_row[pretty_print_j]);
  if pretty_print_j < (Length(pretty_print_row) - 1) then begin
  pretty_print_line := pretty_print_line + '        ';
end;
  pretty_print_j := pretty_print_j + 1;
end;
  writeln(pretty_print_line);
  writeln('');
  pretty_print_i := pretty_print_i + 1;
end;
  writeln('         System Resource Table');
  pretty_print_i := 0;
  while pretty_print_i < Length(max) do begin
  pretty_print_row := max[pretty_print_i];
  pretty_print_line := ('P' + IntToStr(pretty_print_i + 1)) + '       ';
  pretty_print_j := 0;
  while pretty_print_j < Length(pretty_print_row) do begin
  pretty_print_line := pretty_print_line + IntToStr(pretty_print_row[pretty_print_j]);
  if pretty_print_j < (Length(pretty_print_row) - 1) then begin
  pretty_print_line := pretty_print_line + '        ';
end;
  pretty_print_j := pretty_print_j + 1;
end;
  writeln(pretty_print_line);
  writeln('');
  pretty_print_i := pretty_print_i + 1;
end;
  pretty_print_usage := '';
  pretty_print_i := 0;
  while pretty_print_i < Length(claim) do begin
  if pretty_print_i > 0 then begin
  pretty_print_usage := pretty_print_usage + ' ';
end;
  pretty_print_usage := pretty_print_usage + IntToStr(claim[pretty_print_i]);
  pretty_print_i := pretty_print_i + 1;
end;
  pretty_print_alloc_sum := processes_resource_summation(alloc);
  pretty_print_avail := available_resources(claim, pretty_print_alloc_sum);
  pretty_print_avail_str := '';
  pretty_print_i := 0;
  while pretty_print_i < Length(pretty_print_avail) do begin
  if pretty_print_i > 0 then begin
  pretty_print_avail_str := pretty_print_avail_str + ' ';
end;
  pretty_print_avail_str := pretty_print_avail_str + IntToStr(pretty_print_avail[pretty_print_i]);
  pretty_print_i := pretty_print_i + 1;
end;
  writeln('Current Usage by Active Processes: ' + pretty_print_usage);
  writeln('Initial Available Resources:       ' + pretty_print_avail_str);
end;
procedure bankers_algorithm(claim: IntArray; alloc: IntArrayArray; max: IntArrayArray);
var
  bankers_algorithm_need_list: IntArrayArray;
  bankers_algorithm_alloc_sum: IntArray;
  bankers_algorithm_avail: IntArray;
  bankers_algorithm_finished: array of boolean;
  bankers_algorithm_i: integer;
  bankers_algorithm_remaining: integer;
  bankers_algorithm_safe: boolean;
  bankers_algorithm_p: integer;
  bankers_algorithm_exec: boolean;
  bankers_algorithm_r: integer;
  bankers_algorithm_avail_str: string;
begin
  bankers_algorithm_need_list := need(max, alloc);
  bankers_algorithm_alloc_sum := processes_resource_summation(alloc);
  bankers_algorithm_avail := available_resources(claim, bankers_algorithm_alloc_sum);
  writeln('__________________________________________________');
  writeln('');
  bankers_algorithm_finished := [];
  bankers_algorithm_i := 0;
  while bankers_algorithm_i < Length(bankers_algorithm_need_list) do begin
  bankers_algorithm_finished := concat(bankers_algorithm_finished, [false]);
  bankers_algorithm_i := bankers_algorithm_i + 1;
end;
  bankers_algorithm_remaining := Length(bankers_algorithm_need_list);
  while bankers_algorithm_remaining > 0 do begin
  bankers_algorithm_safe := false;
  bankers_algorithm_p := 0;
  while bankers_algorithm_p < Length(bankers_algorithm_need_list) do begin
  if not bankers_algorithm_finished[bankers_algorithm_p] then begin
  bankers_algorithm_exec := true;
  bankers_algorithm_r := 0;
  while bankers_algorithm_r < Length(bankers_algorithm_avail) do begin
  if bankers_algorithm_need_list[bankers_algorithm_p][bankers_algorithm_r] > bankers_algorithm_avail[bankers_algorithm_r] then begin
  bankers_algorithm_exec := false;
  break;
end;
  bankers_algorithm_r := bankers_algorithm_r + 1;
end;
  if bankers_algorithm_exec then begin
  bankers_algorithm_safe := true;
  writeln(('Process ' + IntToStr(bankers_algorithm_p + 1)) + ' is executing.');
  bankers_algorithm_r := 0;
  while bankers_algorithm_r < Length(bankers_algorithm_avail) do begin
  bankers_algorithm_avail[bankers_algorithm_r] := bankers_algorithm_avail[bankers_algorithm_r] + alloc[bankers_algorithm_p][bankers_algorithm_r];
  bankers_algorithm_r := bankers_algorithm_r + 1;
end;
  bankers_algorithm_avail_str := '';
  bankers_algorithm_r := 0;
  while bankers_algorithm_r < Length(bankers_algorithm_avail) do begin
  if bankers_algorithm_r > 0 then begin
  bankers_algorithm_avail_str := bankers_algorithm_avail_str + ' ';
end;
  bankers_algorithm_avail_str := bankers_algorithm_avail_str + IntToStr(bankers_algorithm_avail[bankers_algorithm_r]);
  bankers_algorithm_r := bankers_algorithm_r + 1;
end;
  writeln('Updated available resource stack for processes: ' + bankers_algorithm_avail_str);
  writeln('The process is in a safe state.');
  writeln('');
  bankers_algorithm_finished[bankers_algorithm_p] := true;
  bankers_algorithm_remaining := bankers_algorithm_remaining - 1;
end;
end;
  bankers_algorithm_p := bankers_algorithm_p + 1;
end;
  if not bankers_algorithm_safe then begin
  writeln('System in unsafe state. Aborting...');
  writeln('');
  break;
end;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  claim_vector := [8, 5, 9, 7];
  allocated_resources_table := [[2, 0, 1, 1], [0, 1, 2, 1], [4, 0, 0, 3], [0, 2, 1, 0], [1, 0, 3, 0]];
  maximum_claim_table := [[3, 2, 1, 4], [0, 2, 5, 2], [5, 1, 0, 5], [1, 5, 3, 0], [3, 0, 3, 3]];
  pretty_print(claim_vector, allocated_resources_table, maximum_claim_table);
  bankers_algorithm(claim_vector, allocated_resources_table, maximum_claim_table);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
