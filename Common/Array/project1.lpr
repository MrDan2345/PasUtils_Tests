program Project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Run;
  var Arr: specialize TUArray<Int32>;
  procedure PrintArr;
    var i: Int32;
  begin
    for i := 0 to Arr.LastIndex - 1 do
    begin
      Write(Arr.Data[i], ', ');
    end;
    i := Arr.LastIndex;
    if i > -1 then Write(Arr.Data[i]);
    WriteLn;
  end;
  var i: Int32;
begin
  Randomize;
  for i := 0 to 10 do
  begin
    Arr.Add(Random(10));
  end;
  WriteLn('Array:');
  PrintArr;
  WriteLn('Sorted:');
  Arr.Sort;
  PrintArr;
  WriteLn('Reversed:');
  Arr.Reverse;
  PrintArr;
end;

begin
  Run;
  {$if defined(windows)}
  ReadLn;
  {$endif}
end.

