program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Expect(const TestName: string; const Got, Want: Int32);
begin
  if Got = Want then
  begin
    WriteLn('  PASS  ', TestName);
  end
  else
  begin
    WriteLn('  FAIL  ', TestName, '  got=', Got, '  want=', Want);
  end;
end;

procedure Expect(const TestName: string; const Got, Want: Boolean);
begin
  if Got = Want then
  begin
    WriteLn('  PASS  ', TestName);
  end
  else
  begin
    WriteLn('  FAIL  ', TestName, '  got=', Got, '  want=', Want);
  end;
end;


procedure BasicTest;
  var Heap: specialize TUHeap<Int32>;
begin
  WriteLn('Basic Test');
  Heap.Push([5, 3, 7, 1, 4]);
  Expect('Size after 5 pushes', Heap.Count, 5);
  Expect('Pop 1st (min=1)', Heap.Pop, 1);
  Expect('Pop 2nd (min=3)', Heap.Pop, 3);
  Expect('Pop 3rd (min=4)', Heap.Pop, 4);
  Expect('Pop 4th (min=5)', Heap.Pop, 5);
  Expect('Pop 5th (min=7)', Heap.Pop, 7);
  Expect('Empty after all pops', Heap.IsEmpty, True);
  WriteLn;
end;

procedure HeapSortTest;
  var Heap: specialize TUHeap<Int32>;
begin
  WriteLn('Heap Sort Test');
  Heap.Push([42, -7, 100, 0, 13, -99, 1, 50, 25, -1]);
  Write('  Order: ', Heap.Pop);
  while not Heap.IsEmpty do
  begin
    Write(', ', Heap.Pop);
  end;
  WriteLn;
  WriteLn;
end;

procedure DuplicatesTest;
  var Heap: specialize TUHeap<Int32>;
begin
  WriteLn('Duplicates Test');
  Heap.Push([3, 3, 1, 1, 2]);
  Expect('Pop dup 1st', Heap.Pop, 1);
  Expect('Pop dup 2nd', Heap.Pop, 1);
  Expect('Pop dup 3rd', Heap.Pop, 2);
  Expect('Pop dup 4th', Heap.Pop, 3);
  Expect('Pop dup 5th', Heap.Pop, 3);
  WriteLn;
end;

procedure MaxHeapTest;
  var Heap: specialize TUHeap<Int32>;
begin
  WriteLn('Max Heap Test');
  Heap.Push([42, -7, 100, 0, 13, -99, 1, 50, 25, -1]);
  Heap.MaxHeap := True;
  Write('  Order: ', Heap.Pop);
  while not Heap.IsEmpty do
  begin
    Write(', ', Heap.Pop);
  end;
  WriteLn;
  WriteLn;
end;

procedure Run;
begin
  BasicTest;
  HeapSortTest;
  DuplicatesTest;
  MaxHeapTest;
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

