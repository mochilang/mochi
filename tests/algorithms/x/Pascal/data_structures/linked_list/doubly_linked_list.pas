{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type DoublyLinkedList = record
  data: array of int64;
end;
type IntArray = array of int64;
type DeleteResult = record
  list: DoublyLinkedList;
  value: int64;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeDeleteResult(list: DoublyLinkedList; value: int64): DeleteResult; forward;
function makeDoublyLinkedList(data: IntArray): DoublyLinkedList; forward;
function empty_list(): DoublyLinkedList; forward;
function length_(length__list: DoublyLinkedList): int64; forward;
function is_empty(is_empty_list: DoublyLinkedList): boolean; forward;
function to_string(to_string_list: DoublyLinkedList): string; forward;
function insert_nth(insert_nth_list: DoublyLinkedList; insert_nth_index: int64; insert_nth_value: int64): DoublyLinkedList; forward;
function insert_head(insert_head_list: DoublyLinkedList; insert_head_value: int64): DoublyLinkedList; forward;
function insert_tail(insert_tail_list: DoublyLinkedList; insert_tail_value: int64): DoublyLinkedList; forward;
function delete_nth(delete_nth_list: DoublyLinkedList; delete_nth_index: int64): DeleteResult; forward;
function delete_head(delete_head_list: DoublyLinkedList): DeleteResult; forward;
function delete_tail(delete_tail_list: DoublyLinkedList): DeleteResult; forward;
function delete_value(delete_value_list: DoublyLinkedList; delete_value_value: int64): DeleteResult; forward;
procedure main(); forward;
function makeDeleteResult(list: DoublyLinkedList; value: int64): DeleteResult;
begin
  Result.list := list;
  Result.value := value;
end;
function makeDoublyLinkedList(data: IntArray): DoublyLinkedList;
begin
  Result.data := data;
end;
function empty_list(): DoublyLinkedList;
begin
  exit(makeDoublyLinkedList([]));
end;
function length_(length__list: DoublyLinkedList): int64;
begin
  exit(Length(length__list.data));
end;
function is_empty(is_empty_list: DoublyLinkedList): boolean;
begin
  exit(Length(is_empty_list.data) = 0);
end;
function to_string(to_string_list: DoublyLinkedList): string;
var
  to_string_s: string;
  to_string_i: int64;
begin
  if Length(to_string_list.data) = 0 then begin
  exit('');
end;
  to_string_s := IntToStr(to_string_list.data[0]);
  to_string_i := 1;
  while to_string_i < Length(to_string_list.data) do begin
  to_string_s := (to_string_s + '->') + IntToStr(to_string_list.data[to_string_i]);
  to_string_i := to_string_i + 1;
end;
  exit(to_string_s);
end;
function insert_nth(insert_nth_list: DoublyLinkedList; insert_nth_index: int64; insert_nth_value: int64): DoublyLinkedList;
var
  insert_nth_res: array of int64;
  insert_nth_i: int64;
begin
  if (insert_nth_index < 0) or (insert_nth_index > Length(insert_nth_list.data)) then begin
  panic('index out of range');
end;
  insert_nth_res := [];
  insert_nth_i := 0;
  while insert_nth_i < insert_nth_index do begin
  insert_nth_res := concat(insert_nth_res, IntArray([insert_nth_list.data[insert_nth_i]]));
  insert_nth_i := insert_nth_i + 1;
end;
  insert_nth_res := concat(insert_nth_res, IntArray([insert_nth_value]));
  while insert_nth_i < Length(insert_nth_list.data) do begin
  insert_nth_res := concat(insert_nth_res, IntArray([insert_nth_list.data[insert_nth_i]]));
  insert_nth_i := insert_nth_i + 1;
end;
  exit(makeDoublyLinkedList(insert_nth_res));
end;
function insert_head(insert_head_list: DoublyLinkedList; insert_head_value: int64): DoublyLinkedList;
begin
  exit(insert_nth(insert_head_list, 0, insert_head_value));
end;
function insert_tail(insert_tail_list: DoublyLinkedList; insert_tail_value: int64): DoublyLinkedList;
begin
  exit(insert_nth(insert_tail_list, Length(insert_tail_list.data), insert_tail_value));
end;
function delete_nth(delete_nth_list: DoublyLinkedList; delete_nth_index: int64): DeleteResult;
var
  delete_nth_res: array of int64;
  delete_nth_i: int64;
  delete_nth_removed: int64;
begin
  if (delete_nth_index < 0) or (delete_nth_index >= Length(delete_nth_list.data)) then begin
  panic('index out of range');
end;
  delete_nth_res := [];
  delete_nth_i := 0;
  delete_nth_removed := 0;
  while delete_nth_i < Length(delete_nth_list.data) do begin
  if delete_nth_i = delete_nth_index then begin
  delete_nth_removed := delete_nth_list.data[delete_nth_i];
end else begin
  delete_nth_res := concat(delete_nth_res, IntArray([delete_nth_list.data[delete_nth_i]]));
end;
  delete_nth_i := delete_nth_i + 1;
end;
  exit(makeDeleteResult(makeDoublyLinkedList(delete_nth_res), delete_nth_removed));
end;
function delete_head(delete_head_list: DoublyLinkedList): DeleteResult;
begin
  exit(delete_nth(delete_head_list, 0));
end;
function delete_tail(delete_tail_list: DoublyLinkedList): DeleteResult;
begin
  exit(delete_nth(delete_tail_list, Length(delete_tail_list.data) - 1));
end;
function delete_value(delete_value_list: DoublyLinkedList; delete_value_value: int64): DeleteResult;
var
  delete_value_idx: int64;
  delete_value_found: boolean;
begin
  delete_value_idx := 0;
  delete_value_found := false;
  while delete_value_idx < Length(delete_value_list.data) do begin
  if delete_value_list.data[delete_value_idx] = delete_value_value then begin
  delete_value_found := true;
  break;
end;
  delete_value_idx := delete_value_idx + 1;
end;
  if not delete_value_found then begin
  panic('value not found');
end;
  exit(delete_nth(delete_value_list, delete_value_idx));
end;
procedure main();
var
  main_dll: DoublyLinkedList;
  main_res: DeleteResult;
begin
  main_dll := empty_list();
  main_dll := insert_tail(main_dll, 1);
  main_dll := insert_tail(main_dll, 2);
  main_dll := insert_tail(main_dll, 3);
  writeln(to_string(main_dll));
  main_dll := insert_head(main_dll, 0);
  writeln(to_string(main_dll));
  main_dll := insert_nth(main_dll, 2, 9);
  writeln(to_string(main_dll));
  main_res := delete_nth(main_dll, 2);
  main_dll := main_res.list;
  writeln(main_res.value);
  writeln(to_string(main_dll));
  main_res := delete_tail(main_dll);
  main_dll := main_res.list;
  writeln(main_res.value);
  writeln(to_string(main_dll));
  main_res := delete_value(main_dll, 1);
  main_dll := main_res.list;
  writeln(main_res.value);
  writeln(to_string(main_dll));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
