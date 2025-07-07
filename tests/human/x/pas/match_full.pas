program MatchFull;

function classify(n: integer): string;
begin
  case n of
    0: classify := 'zero';
    1: classify := 'one';
  else
    classify := 'many';
  end;
end;

var
  x: integer;
  day: string;
  mood: string;
  ok: boolean;
  labelStr: string;
begin
  x := 2;
  case x of
    1: labelStr := 'one';
    2: labelStr := 'two';
    3: labelStr := 'three';
  else
    labelStr := 'unknown';
  end;
  Writeln(labelStr);

  day := 'sun';
  if day = 'mon' then mood := 'tired'
  else if day = 'fri' then mood := 'excited'
  else if day = 'sun' then mood := 'relaxed'
  else mood := 'normal';
  Writeln(mood);

  ok := true;
  if ok then labelStr := 'confirmed' else labelStr := 'denied';
  Writeln(labelStr);

  Writeln(classify(0));
  Writeln(classify(5));
end.
