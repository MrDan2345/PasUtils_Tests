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

procedure TestReplacePairs(const Input, Expected: String; const Pairs: array of TUStrPair);
  var Actual: String;
begin
  Actual := UStrReplace(Input, Pairs);
  if Actual = Expected then
  begin
    WriteLn('[PASS] ', Copy(Input, 1, 10), '... -> ', Actual)
  end
  else
  begin
    WriteLn('[FAIL] Input: "', Input, '" | Expected: "', Expected, '" | Actual: "', Actual, '"');
  end;
end;

procedure Run2;
begin
  WriteLn('--------------------------------------------------');
  TestReplacePairs('Hello World', 'Help Pascal', [['World', 'Pascal'], ['lo', 'p']]);
  TestReplacePairs('\"hello world\"\\\\', '"hello world"\\', [['\r', #$d], ['\"', '"'], ['\t', #$9], ['\\', '\']]);
  TestReplacePairs('AB', 'AA', [['B', 'A']]);
  TestReplacePairs('banana', 'bonono', [['a', 'o']]);
  TestReplacePairs('Apple Pie', 'Apple Pie', [['Orange', 'Grape']]);
  TestReplacePairs('The cat sat', 'A cat sat', [['The', 'A']]);
  TestReplacePairs('I love Pascal', 'I love Coding', [['Pascal', 'Coding']]);
  TestReplacePairs('', '', [['test', 'best']]);
  TestReplacePairs('Remove the space', 'Removethespace', [[' ', '']]);
  TestReplacePairs('Short', 'Short', [['Very Long Pattern', 'X']]);
  TestReplacePairs('ShortString', 'ShortString', [['ShortStringButLongerThanOriginal', 'X']]);
  TestReplacePairs('Hello', 'Hello', [['', 'World']]);
  TestReplacePairs('apple', 'aapple', [['a', 'aa']]);
  TestReplacePairs('ABC', 'ACB', [['BC', 'CB']]);
  TestReplacePairs('AAAA', 'XX', [['AA', 'X']]);
  TestReplacePairs('123', 'One Two Three', [['1', 'One '], ['2', 'Two '], ['3', 'Three']]);
  TestReplacePairs('The quick brown fox', 'The fox', [['quick brown ', '']]);
  TestReplacePairs('START middle END', 'BEGIN middle FINISH', [['START', 'BEGIN'], ['END', 'FINISH']]);
  TestReplacePairs('aaa', 'ba', [['aa', 'b']]);
  TestReplacePairs('The A is B', 'The LONG_WORD is S', [['A', 'LONG_WORD'], ['B', 'S']]);
  WriteLn('--------------------------------------------------');
end;

begin
  Run2;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

