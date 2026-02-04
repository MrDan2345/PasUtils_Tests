program FastList;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Run;
  type TItem = record
    var Ch: AnsiChar;
    var Id: Int32;
  end;
  var Item: TItem;
  var Items: array of TItem;
  var List: specialize TUFastList<AnsiChar>;
  procedure LogLine;
  begin
    WriteLn('-----------------------------');
  end;
  procedure LogList;
    var i: Int32;
  begin
    {
    for i := 0 to High(List._Ind) do
    begin
      Write(i, ' ');
    end;
    WriteLn;
    for i := 0 to High(List._Ind) do
    begin
      Write('- ');
    end;
    WriteLn;
    for i := 0 to High(List._Ind) do
    begin
      Write(List._Ind[i].Data, ' ');
    end;
    WriteLn;
    for i := 0 to High(List._Ind) do
    begin
      Write(List._Ind[i].Id, ' ');
    end;
    WriteLn;
    }
    for i := 0 to List.LastIndex do
    begin
      Write(List.DataRaw[i], ' ');
    end;
    WriteLn;
    LogLine;
  end;
  procedure LogArray;
    var i: Int32;
  begin
    WriteLn('Array:');
    for i := 0 to High(Items) do
    begin
      Write(Items[i].Ch, '[', Items[i].Id, '] ');
    end;
    WriteLn;
    LogLine;
  end;
  procedure LogListIndexed;
    var i: Int32;
  begin
    WriteLn('List Indexed:');
    for i := 0 to High(Items) do
    begin
      Write(List[Items[i].Id], '[', Items[i].Id, '] ');
    end;
    WriteLn;
    LogLine;
  end;
  procedure Verify;
    var i, n: Int32;
  begin
    {
    for i := 0 to List.LastIndex do
    begin
      n := List._Ind[i].Id;
      n := List._Ind[n].Data;
      if n > List.LastIndex then Continue;
      if n = i then Continue;
      WriteLn('Error!');
      WriteLn('Index Mismatch: n = ', n, ' i = ', i);
    end;
    for i := 0 to High(Items) do
    begin
      if Items[i].Ch = List[Items[i].Id] then Continue;
      WriteLn('Error!');
      LogArray;
      WriteLn('Array Mismatch: i = ', i, ' Array Id = ', Items[i].Id, ' List Id = ', List._Ind[Items[i].Id].Data);
    end;
    }
  end;
  var i, j: Int32;
begin
  //List.Slack := 0;
  WriteLn('Fast Unordered List Test');
  LogLine;
  Items := nil;
  Randomize;
  for i := 0 to 5 do
  begin
    Item.Ch := AnsiChar(Ord('A') + i);
    Item.Id := List.Add(Item.Ch);
    specialize UArrAppend<TItem>(Items, Item);
  end;
  LogList;
  for i := 0 to 9 do
  begin
    if (Length(Items) > 2) and (Random(2) = 1) then
    begin
      j := Random(Length(Items));
      Assert(Items[j].Id = List.Id[j]);
      WriteLn('Deleting ', Items[j].Ch, ' [', Items[j].Id, ']');
      List.Delete(Items[j].Id);
      specialize UArrDelete<TItem>(Items, j);
    end
    else
    begin
      Item.Ch := AnsiChar(Ord('a') + Random(26));
      Item.Id := List.Add(Item.Ch);
      WriteLn('Adding ', Item.Ch, ' [', Item.Id, ']');
      specialize UArrAppend<TItem>(Items, Item);
    end;
    LogList;
    Verify;
  end;
  for i := 0 to List.LastIndex - 1 do
  for j := i + 1 to List.LastIndex do
  begin
    if List.Id[i] <> List.Id[j] then Continue;
    WriteLn('Error!');
  end;
  LogList;
  LogArray;
  LogListIndexed;
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

