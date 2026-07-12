program Project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure TestArrayInsert;
  var Arr1, Arr2: specialize TArray<Int32>;
  var i: Int32;
begin
  Arr1 := [1, 2, 3, 4, 5];
  Arr2 := [8, 9];
  UArrInsert(Arr1, Arr2, 1);
  for i := 0 to High(Arr1) do
  begin
    if i > 0 then Write(', ');
    Write(Arr1[i]);
  end;
  WriteLn;
end;

procedure Run;
begin
  Randomize;
  TestArrayInsert;
end;

begin
  Run;
  {$if defined(windows)}
  ReadLn;
  {$endif}
end.

