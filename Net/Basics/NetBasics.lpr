program NetBasics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  {$if defined(windows)}
  //WinSock2,
  {$else}
  {$endif}
  NetUtils;

procedure Run;
  var Addr: TUInAddr;
begin
  WriteLn('Host Name: ', UNetHostName);
  Addr := UNetLocalAddr;
  WriteLn('Local Addr (Net): ', UNetNetAddrToStr(Addr));
  Addr := UNetNetToHost(Addr);
  WriteLn('Local Addr (Host): ', UNetHostAddrToStr(Addr));
end;

begin
  Run;
  WriteLn('Done');
  ReadLn;
end.

