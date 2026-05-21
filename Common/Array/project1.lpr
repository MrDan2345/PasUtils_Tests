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
  Arr.Add([1, 2, 3]);
  for i := 0 to 15 do
  begin
    Arr.Add(Random(10));
  end;
  WriteLn('Array:');
  PrintArr;
  WriteLn('Delete 3 [1]');
  Arr.Delete(1, 3);
  PrintArr;
  WriteLn('Remove all 7');
  Arr.RemoveAll(7);
  PrintArr;
  WriteLn('Insert -5 at 3');
  Arr.Insert(-5, 3);
  PrintArr;
  WriteLn('Insert -1 at -3');
  Arr.Insert(-1, -3);
  PrintArr;
  WriteLn('Sorted:');
  Arr.Sort;
  PrintArr;
  WriteLn('Reversed:');
  Arr.Reverse;
  PrintArr;
  i := Random(10);
  WriteLn('Find ', i, ' = ', Arr.Find(i));
  i := -((Random(Arr.Count) div 2) + 1);
  WriteLn('Negative Index [', i, '] = ', Arr[i]);
  Write('Enumerator loop: ');
  for i in Arr do Write(i, ' ');
  WriteLn;
end;

begin
  Run;
  {$if defined(windows)}
  ReadLn;
  {$endif}
end.

