program ecc_edwards;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CryptoUtils,
  CommonUtils;

procedure Test;
begin
  if (False) then
  begin
    WriteLn('✅ ... PASSED!')
  end
  else
  begin
    WriteLn('❌ ... FAILED!');
  end;
end;

begin
  Test;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

