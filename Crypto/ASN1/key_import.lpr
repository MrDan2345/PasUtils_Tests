program key_import;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function ImportKey(const KeyStr: String): Boolean;
  var Key: TURSA.TKey;
begin
  Key := TURSA.ImportKey(KeyStr);
  Result := Key.IsValid;
  if not Result then Exit;
  if Key.IsPrivate then
  begin
    WriteLn('Private Key');
  end
  else
  begin
    WriteLn('Public Key');
  end;
  WriteLn('Key Length: ', Key.Size);
end;

procedure Run;
  var KeyFiles: TUStrArray;
  var i: Int32;
begin
  KeyFiles := UFileSearch(UAppPath + '/keys/*.txt');
  WriteLn(Length(KeyFiles), ' Keys found.');
  for i := 0 to High(KeyFiles) do
  begin
    WriteLn('Import: ', KeyFiles[i]);
    if ImportKey(UFileToStr(UAppPath + '/keys/' + KeyFiles[i])) then
    begin
      WriteLn('SUCCESS');
    end
    else
    begin
      WriteLn('FAIL');
    end;
    WriteLn();
  end;
  WriteLn('Done.');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

