program Main;

const
  AnswerCount = 6;
  Answers: array[1..AnswerCount] of string = (
    'One Hundred Twenty Three',
    'Twelve Thousand Three Hundred Forty Five',
    'One Million Two Hundred Thirty Four Thousand Five Hundred Sixty Seven',
    'Zero',
    'One Million Ten',
    'Two Billion One Hundred Forty Seven Million Four Hundred Eighty Three Thousand Six Hundred Forty Seven'
  );

var
  t, i, num: LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do
  begin
    ReadLn(num);
    if i > 1 then WriteLn;
    Write(Answers[i]);
  end;
end.
