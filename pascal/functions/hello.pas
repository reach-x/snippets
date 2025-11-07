program HelloWorld;
var
  i: integer;
  numbers: array[1..5] of integer;
begin
  WriteLn('Hello from Pascal!');
  for i := 1 to 5 do
    numbers[i] := i * i;
  for i := 1 to 5 do
    WriteLn(numbers[i]);
end.
