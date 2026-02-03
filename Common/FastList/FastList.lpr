program FastList;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Run;
  var List: specialize TUFastList<AnsiChar>;
  procedure LogLine;
  begin
    WriteLn('-----------------------------');
  end;
  procedure LogList;
    var i: Int32;
  begin
    for i := 0 to List.LastIndex do
    begin
      Write(List[i], '[', List.Id[i], '] ');
    end;
    WriteLn;
    LogLine;
  end;
  type TItem = record
    var Ch: AnsiChar;
    var Id: Int32;
  end;
  var Item: TItem;
  var Items: array of TItem;
  var i, j: Int32;
begin
  WriteLn('Fast Unordered List Test');
  LogLine;
  Items := nil;
  Randomize;
  for i := 0 to 25 do
  begin
    Item.Ch := AnsiChar(Ord('A') + i);
    Item.Id := List.Add(Item.Ch);
    specialize UArrAppend<TItem>(Items, Item);
  end;
  LogList;
  for i := 0 to 20 do
  begin
    if Random(2) = 1 then
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
  end;
  for i := 0 to List.LastIndex - 1 do
  for j := i + 1 to List.LastIndex do
  begin
    Assert(List.Id[i] <> List.Id[j]);
  end;
  LogList;
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

