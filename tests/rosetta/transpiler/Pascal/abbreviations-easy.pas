{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
var
  fields_words: array of string;
  fields_cur: string;
  fields_i: integer;
  fields_ch: string;
  padRight_out: string;
  padRight_i: integer;
  join_res: string;
  join_i: integer;
  validate_results: array of string;
  validate_wi: integer;
  validate_w: string;
  validate_found: boolean;
  validate_wlen: integer;
  validate_ci: integer;
  validate_cmd: string;
  validate_c: string;
  validate_ww: string;
  main_table: string;
  main_commands: StrArray;
  main_mins: array of integer;
  main_i: integer;
  main_count: integer;
  main_j: integer;
  main_cmd: string;
  main_ch: string;
  main_sentence: string;
  main_words: StrArray;
  main_results: StrArray;
  main_out1: string;
  main_k: integer;
function fields(s: string): StrArray;
begin
  fields_words := [];
  fields_cur := '';
  fields_i := 0;
  while fields_i < Length(s) do begin
  fields_ch := copy(s, fields_i+1, (fields_i + 1 - (fields_i)));
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
function padRight(s: string; width: integer): string;
begin
  padRight_out := s;
  padRight_i := Length(s);
  while padRight_i < width do begin
  padRight_out := padRight_out + ' ';
  padRight_i := padRight_i + 1;
end;
  exit(padRight_out);
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
function validate(commands: StrArray; words: StrArray; mins: IntArray): StrArray;
begin
  validate_results := [];
  if Length(words) = 0 then begin
  exit(validate_results);
end;
  validate_wi := 0;
  while validate_wi < Length(words) do begin
  validate_w := words[validate_wi];
  validate_found := false;
  validate_wlen := Length(validate_w);
  validate_ci := 0;
  while validate_ci < Length(commands) do begin
  validate_cmd := commands[validate_ci];
  if ((mins[validate_ci] <> 0) and (validate_wlen >= mins[validate_ci])) and (validate_wlen <= Length(validate_cmd)) then begin
  validate_c := UpperCase(validate_cmd);
  validate_ww := UpperCase(validate_w);
  if copy(validate_c, 0+1, (validate_wlen - (0))) = validate_ww then begin
  validate_results := concat(validate_results, [validate_c]);
  validate_found := true;
  break;
end;
end;
  validate_ci := validate_ci + 1;
end;
  if not validate_found then begin
  validate_results := concat(validate_results, ['*error*']);
end;
  validate_wi := validate_wi + 1;
end;
  exit(validate_results);
end;
procedure main();
begin
  main_table := ((((('Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy ' + 'COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ') + 'NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ') + ' Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ') + 'MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ') + 'READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ') + 'RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp ';
  main_commands := fields(main_table);
  main_mins := [];
  main_i := 0;
  while main_i < Length(main_commands) do begin
  main_count := 0;
  main_j := 0;
  main_cmd := main_commands[main_i];
  while main_j < Length(main_cmd) do begin
  main_ch := copy(main_cmd, main_j+1, (main_j + 1 - (main_j)));
  if (main_ch >= 'A') and (main_ch <= 'Z') then begin
  main_count := main_count + 1;
end;
  main_j := main_j + 1;
end;
  main_mins := concat(main_mins, [main_count]);
  main_i := main_i + 1;
end;
  main_sentence := 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin';
  main_words := fields(main_sentence);
  main_results := validate(main_commands, main_words, main_mins);
  main_out1 := 'user words:  ';
  main_k := 0;
  while main_k < Length(main_words) do begin
  main_out1 := (main_out1 + padRight(main_words[main_k], Length(main_results[main_k]))) + ' ';
  main_k := main_k + 1;
end;
  writeln(main_out1);
  writeln('full words:  ' + join(main_results, ' '));
end;
begin
  main();
end.
