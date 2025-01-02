program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  NetUtils;

procedure Run;
  var Addr: TUInAddr;
begin
  Addr := UNetStrToNetAddr('192.168.1.119');
  WriteLn('Ping ', UNetNetAddrToStr(Addr), ' ', UBoolToStr(UNetPing(Addr), 'Success', 'Failed'));
end;

begin
  Run;
  ReadLn;
end.

