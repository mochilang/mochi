// Generated by Mochi compiler v0.10.26 on 2025-07-16T11:36:15Z
program BitmapReadAPpmFile;
{$mode objfpc}
{$modeswitch nestedprocvars}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;
type Pixel = record
  R: integer;
  G: integer;
  B: integer;
end;
type Bitmap = record
  w: integer;
  h: integer;
  max: integer;
  data: specialize TArray<specialize TArray<Pixel>>;
end;

function newBitmap(w: integer; h: integer; max: integer): Bitmap;
var
  _tmp0: Pixel;
  _tmp1: Bitmap;
  row: specialize TArray<integer>;
  rows: specialize TArray<specialize TArray<integer>>;
  x: integer;
  y: integer;
begin
  rows := specialize TArray<specialize TArray<integer>>([]);
  y := 0;
  while (y < h) do
  begin
    row := specialize TArray<integer>([]);
    x := 0;
    while (x < w) do
    begin
      _tmp0.R := 0;
      _tmp0.G := 0;
      _tmp0.B := 0;
      row := specialize _appendList<Variant>(row, _tmp0);
      x := x + 1;
    end;
    rows := specialize _appendList<Variant>(rows, row);
    y := y + 1;
  end;
  _tmp1.w := w;
  _tmp1.h := h;
  _tmp1.max := max;
  _tmp1.data := rows;
  result := _tmp1;
  exit;
end;

function setPx(b: Bitmap; x: integer; y: integer; p: Pixel): integer;
var
  row: specialize TArray<integer>;
  rows: specialize TArray<specialize TArray<integer>>;
begin
  rows := b.data;
  row := specialize _indexList<specialize TArray<integer>>(rows, y);
  row[x] := p;
  rows[y] := row;
  b := rows;
end;

function getPx(b: Bitmap; x: integer; y: integer): Pixel;
begin
  result := b.data[y][x];
  exit;
end;

function splitLines(s: string): specialize TArray<string>;
var
  ch: Variant;
  cur: string;
  i: integer;
  out: string;
begin
  out := specialize TArray<integer>([]);
  cur := '';
  i := 0;
  while (i < Length(s)) do
  begin
    ch := _sliceString(s, i, i + i + 1);
    if (ch = '
') then
    begin
      out := specialize _appendList<Variant>(out, cur);
      cur := '';
    end else
    begin
      cur := cur + ch;
    end;
    i := i + 1;
  end;
  out := specialize _appendList<Variant>(out, cur);
  result := out;
  exit;
end;

function splitWS(s: string): specialize TArray<string>;
var
  ch: Variant;
  cur: string;
  i: integer;
  out: string;
begin
  out := specialize TArray<integer>([]);
  cur := '';
  i := 0;
  while (i < Length(s)) do
  begin
    ch := _sliceString(s, i, i + i + 1);
    if ((((ch = ' ') or (ch = '  ')) or (ch = '')) or (ch = '
')) then
    begin
      if (Length(cur) > 0) then
      begin
        out := specialize _appendList<Variant>(out, cur);
        cur := '';
      end;
    end else
    begin
      cur := cur + ch;
    end;
    i := i + 1;
  end;
  if (Length(cur) > 0) then ;
  result := out;
  exit;
end;

function parseIntStr(str: string): integer;
var
  _tmp2: specialize TFPGMap<string, integer>;
  digits: specialize TFPGMap<Variant, Variant>;
  i: integer;
  n: integer;
  neg: boolean;
begin
  i := 0;
  neg := False;
  if ((Length(str) > 0) and (_sliceString(str, 0, 1) = '-')) then
  begin
    neg := True;
    i := 1;
  end;
  n := 0;
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('0', 0);
  _tmp2.AddOrSetData('1', 1);
  _tmp2.AddOrSetData('2', 2);
  _tmp2.AddOrSetData('3', 3);
  _tmp2.AddOrSetData('4', 4);
  _tmp2.AddOrSetData('5', 5);
  _tmp2.AddOrSetData('6', 6);
  _tmp2.AddOrSetData('7', 7);
  _tmp2.AddOrSetData('8', 8);
  _tmp2.AddOrSetData('9', 9);
  digits := _tmp2;
  while (i < Length(str)) do
  begin
    n := n * 10 + digits[_sliceString(str, i, i + 1)];
    i := i + 1;
  end;
  if neg then ;
  result := n;
  exit;
end;

function tokenize(s: string): specialize TArray<string>;
var
  i: integer;
  j: integer;
  line: Variant;
  lines: Variant;
  parts: Variant;
  toks: specialize TArray<string>;
begin
  lines := splitLines(s);
  toks := specialize TArray<string>([]);
  i := 0;
  while (i < Length(lines)) do
  begin
    line := lines[i];
    if ((Length(line) > 0) and (_sliceString(line, 0, 0 + 1) = '#')) then
    begin
      i := i + 1;
      continue;
    end;
    parts := splitWS(line);
    j := 0;
    while (j < Length(parts)) do
    begin
      toks := specialize _appendList<Variant>(toks, parts[j]);
      j := j + 1;
    end;
    i := i + 1;
  end;
  result := toks;
  exit;
end;

function readP3(text: string): Bitmap;
var
  _tmp3: Pixel;
  b: Variant;
  bm: Bitmap;
  g: Variant;
  h: Variant;
  idx: integer;
  maxv: Variant;
  r: Variant;
  toks: specialize TArray<string>;
  w: Variant;
  x: integer;
  y: Variant;
begin
  toks := tokenize(text);
  if (Length(toks) < 4) then ;
  if (specialize _indexList<string>(toks, 0) <> 'P3') then ;
  w := parseIntStr(specialize _indexList<string>(toks, 1));
  h := parseIntStr(specialize _indexList<string>(toks, 2));
  maxv := parseIntStr(specialize _indexList<string>(toks, 3));
  idx := 4;
  bm := newBitmap(w, h, maxv);
  y := h - 1;
  while (y >= 0) do
  begin
    x := 0;
    while (x < w) do
    begin
      r := parseIntStr(specialize _indexList<string>(toks, idx));
      g := parseIntStr(specialize _indexList<string>(toks, idx + 1));
      b := parseIntStr(specialize _indexList<string>(toks, idx + 2));
      _tmp3.R := r;
      _tmp3.G := g;
      _tmp3.B := b;
      setPx(bm, x, y, _tmp3);
      idx := idx + 3;
      x := x + 1;
    end;
    y := y - 1;
  end;
  result := bm;
  exit;
end;

function toGrey(b: Bitmap): integer;
var
  _tmp4: Pixel;
  h: Variant;
  l: Variant;
  m: integer;
  p: Variant;
  w: Variant;
  x: integer;
  y: Variant;
begin
  h := b.h;
  w := b.w;
  m := 0;
  y := 0;
  while (y < h) do
  begin
    x := 0;
    while (x < w) do
    begin
      p := getPx(b, x, y);
      l := p.R * 2126 + p.G * 7152 + p.B * 722 / 10000;
      if (l > b.max) then ;
      _tmp4.R := l;
      _tmp4.G := l;
      _tmp4.B := l;
      setPx(b, x, y, _tmp4);
      if (l > m) then ;
      x := x + 1;
    end;
    y := y + 1;
  end;
  b := m;
end;

function pad(n: integer; w: integer): string;
var
  s: Variant;
begin
  s := IntToStr(n);
  while (Length(s) < w) do
  begin
    s := ' ' + s;
  end;
  result := s;
  exit;
end;

function writeP3(b: Bitmap): string;
var
  digits: specialize TFPGMap<Variant, Variant>;
  h: Variant;
  line: Variant;
  max: function(p0: Variant): Variant is nested;
  out: string;
  p: Variant;
  w: Variant;
  x: integer;
  y: Variant;
begin
  h := b.h;
  w := b.w;
  max := b.max;
  digits := Length(IntToStr(max));
  out := 'P3
# generated from Bitmap.writeppmp3
' + IntToStr(w) + ' ' + IntToStr(h) + '
' + IntToStr(max) + '
';
  y := h - 1;
  while (y >= 0) do
  begin
    line := '';
    x := 0;
    while (x < w) do
    begin
      p := getPx(b, x, y);
      line := line + '   ' + pad(p.R, digits) + ' ' + pad(p.G, digits) + ' ' + pad(p.B, digits);
      x := x + 1;
    end;
    out := out + line + '
';
    y := y - 1;
  end;
  result := out;
  exit;
end;

generic function _appendList<T>(arr: specialize TArray<T>; val: T): specialize TArray<T>;
var i,n: Integer;
begin
  n := Length(arr);
  SetLength(Result, n + 1);
  for i := 0 to n - 1 do
    Result[i] := arr[i];
  Result[n] := val;
end;

generic function _indexList<T>(arr: specialize TArray<T>; i: integer): T;
begin
  if i < 0 then i := Length(arr) + i;
  if (i < 0) or (i >= Length(arr)) then
    raise Exception.Create('index out of range');
  Result := arr[i];
end;

function _sliceString(s: string; i, j: integer): string;
var start_, end_, n: integer;
begin
  start_ := i;
  end_ := j;
  n := Length(s);
  if start_ < 0 then start_ := n + start_;
  if end_ < 0 then end_ := n + end_;
  if start_ < 0 then start_ := 0;
  if end_ > n then end_ := n;
  if end_ < start_ then end_ := start_;
  Result := Copy(s, start_ + 1, end_ - start_);
end;

var
  bm: Bitmap;
  out: string;
  ppmtxt: string;

begin
  ppmtxt := 'P3
' + '# feep.ppm
' + '4 4
' + '15
' + ' 0  0  0    0  0  0    0  0  0   15  0 15
' + ' 0  0  0    0 15  7    0  0  0    0  0  0
' + ' 0  0  0    0  0  0    0 15  7    0  0  0
' + '15  0 15    0  0  0    0  0  0    0  0  0
';
  writeln('Original Colour PPM file');
  writeln(ppmtxt);
  bm := readP3(ppmtxt);
  writeln('Grey PPM:');
  toGrey(bm);
  out := writeP3(bm);
  writeln(out);
end.
