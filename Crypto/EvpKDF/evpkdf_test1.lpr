program evpkdf_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

procedure Run;
begin

end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

