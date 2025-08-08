{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  PI: real;
function sqrtApprox(sqrtApprox_x: real): real; forward;
function atanApprox(atanApprox_x: real): real; forward;
function atan2Approx(atan2Approx_y: real; atan2Approx_x: real): real; forward;
function deg(deg_rad: real): real; forward;
function floor(floor_x: real): real; forward;
function pow10(pow10_n: integer): real; forward;
function round(round_x: real; round_n: integer): real; forward;
function rectangular_to_polar(rectangular_to_polar_real_: real; rectangular_to_polar_img: real): RealArray; forward;
procedure show(show_real_: real; show_img: real); forward;
function sqrtApprox(sqrtApprox_x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := sqrtApprox_x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (sqrtApprox_x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function atanApprox(atanApprox_x: real): real;
begin
  if atanApprox_x > 1 then begin
  exit((PI / 2) - (atanApprox_x / ((atanApprox_x * atanApprox_x) + 0.28)));
end;
  if atanApprox_x < -1 then begin
  exit((-PI / 2) - (atanApprox_x / ((atanApprox_x * atanApprox_x) + 0.28)));
end;
  exit(atanApprox_x / (1 + ((0.28 * atanApprox_x) * atanApprox_x)));
end;
function atan2Approx(atan2Approx_y: real; atan2Approx_x: real): real;
var
  atan2Approx_r: real;
begin
  if atan2Approx_x > 0 then begin
  atan2Approx_r := atanApprox(atan2Approx_y / atan2Approx_x);
  exit(atan2Approx_r);
end;
  if atan2Approx_x < 0 then begin
  if atan2Approx_y >= 0 then begin
  exit(atanApprox(atan2Approx_y / atan2Approx_x) + PI);
end;
  exit(atanApprox(atan2Approx_y / atan2Approx_x) - PI);
end;
  if atan2Approx_y > 0 then begin
  exit(PI / 2);
end;
  if atan2Approx_y < 0 then begin
  exit(-PI / 2);
end;
  exit(0);
end;
function deg(deg_rad: real): real;
begin
  exit((deg_rad * 180) / PI);
end;
function floor(floor_x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(floor_x);
  if Double(floor_i) > floor_x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow10(pow10_n: integer): real;
var
  pow10_p: real;
  pow10_i: integer;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < pow10_n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function round(round_x: real; round_n: integer): real;
var
  round_m: real;
begin
  round_m := pow10(round_n);
  exit(floor((round_x * round_m) + 0.5) / round_m);
end;
function rectangular_to_polar(rectangular_to_polar_real_: real; rectangular_to_polar_img: real): RealArray;
var
  rectangular_to_polar_mod_: real;
  rectangular_to_polar_ang: real;
begin
  rectangular_to_polar_mod_ := round(sqrtApprox((rectangular_to_polar_real_ * rectangular_to_polar_real_) + (rectangular_to_polar_img * rectangular_to_polar_img)), 2);
  rectangular_to_polar_ang := round(deg(atan2Approx(rectangular_to_polar_img, rectangular_to_polar_real_)), 2);
  exit([rectangular_to_polar_mod_, rectangular_to_polar_ang]);
end;
procedure show(show_real_: real; show_img: real);
var
  show_r: RealArray;
begin
  show_r := rectangular_to_polar(show_real_, show_img);
  writeln(list_real_to_str(show_r));
end;
begin
  PI := 3.141592653589793;
  show(5, -5);
  show(-1, 1);
  show(-1, -1);
  show(1e-10, 1e-10);
  show(-1e-10, 1e-10);
  show(9.75, 5.93);
  show(10000, 99999);
end.
