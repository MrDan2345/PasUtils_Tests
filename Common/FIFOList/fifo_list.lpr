program fifo_list;

{$include PasUtilsMode.inc}

uses
  CommonUtils;

procedure Test1;
  var List: specialize TUFIFOList<Int32>;
  var i: Int32;
begin
  List.Cached := True;
  for i := 1 to 5 do
  begin
    List.Enqueue(i);
    WriteLn('Enqueued ', i);
  end;
  while not List.IsEmpty do
  begin
    WriteLn('Dequeued ', List.Dequeue);
  end;
end;

begin
  Test1;
end.

