program project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Run;
  var Stack: specialize TUStack<Int32>;
  var i, n: Int32;
begin
  for i := 0 to 9 do
  begin
    n := Random(100) + 1;
    Stack.Push(n);
    WriteLn('Push ', n);
  end;
  WriteLn('Stack Count = ', Stack.Count);
  for i := 0 to Stack.Count - 1 do
  begin
    n := Stack.Pop;
    WriteLn('Pop ', n);
  end;
  WriteLn('Stack Count = ', Stack.Count);
end;

begin
  Run;
  {$if defined(windows)}
  ReadLn;
  {$endif}
end.

