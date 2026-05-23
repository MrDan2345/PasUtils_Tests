program project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Test(const Input, Pattern, Expected: String);
  var Actual: String;
begin
  Actual := UStrRemove(Input, Pattern);
  if Actual = Expected then
  begin
    WriteLn('[PASS] ', Copy(Input, 1, 10), '... -> ', Actual);
  end
  else
  begin
    WriteLn('[FAIL] Input: "', Input, '" | Expected: "', Expected, '" | Actual: "', Actual, '"');
  end;
end;

procedure Run;
begin
  Test('Hello World', 'xyz', 'Hello World');
  Test('abcABC', 'abc', 'ABC');
  Test('Hello X World', 'X', 'Hello  World');
  Test('Test String', 'String', 'Test ');
  Test('ababa', 'a', 'bb');
  Test('aaaa', 'aa', '');
  Test('', 'a', '');
  Test('hello', '', 'hello');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

