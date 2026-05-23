program project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure TestReplace(const Input, Old, New, Expected: String);
  var Actual: String;
begin
  Actual := UStrReplace(Input, Old, New);
  if Actual = Expected then
  begin
    WriteLn('[PASS] ', Copy(Input, 1, 10), '... -> ', Actual)
  end
  else
  begin
    WriteLn('[FAIL] Input: "', Input, '" | Expected: "', Expected, '" | Actual: "', Actual, '"');
  end;
end;

procedure Run;
begin
  WriteLn('--------------------------------------------------');
  TestReplace('Hello World', 'World', 'Pascal', 'Hello Pascal');
  TestReplace('banana', 'a', 'o', 'bonono');
  TestReplace('Apple Pie', 'Orange', 'Grape', 'Apple Pie');
  TestReplace('The cat sat', 'The', 'A', 'A cat sat');
  TestReplace('I love Pascal', 'Pascal', 'Coding', 'I love Coding');
  TestReplace('', 'test', 'best', '');
  TestReplace('Remove the space', ' ', '', 'Removethespace');
  TestReplace('Short', 'Very Long Pattern', 'X', 'Short');
  TestReplace('ShortString', 'ShortStringButLongerThanOriginal', 'X', 'ShortString');
  TestReplace('Hello', '', 'World', 'Hello');
  TestReplace('apple', 'a', 'aa', 'aapple');
  TestReplace('ABC', 'BC', 'CB', 'ACB');
  TestReplace('AAAA', 'AA', 'X', 'XX');
  WriteLn('--------------------------------------------------');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

