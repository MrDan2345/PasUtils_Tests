program NetBasics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  {$if defined(windows)}
  WinSock2,
  {$else}
  {$endif}
  NetUtils;

type TClient = class (TThread)
public
  procedure Execute; override;
end;

var Client: TClient;

procedure Run;
  var Addr: TUInAddr;
  var SockAddr, OtherSockAddr: TUSockAddr;
  var Sock, SockOther: TUSocket;
  var OtherSockAddrLen: TUSockLen;
begin
  WriteLn('Host Name: ', UNetHostName);
  Addr := UNetLocalAddr;
  WriteLn('Local Addr (Net): ', UNetNetAddrToStr(Addr));
  Addr := UNetNetToHost(Addr);
  WriteLn('Local Addr (Host): ', UNetHostAddrToStr(Addr));
  SockAddr := TUSockAddr.Default;
  SockAddr.sin_port := UNetHostToNetShort(5921);
  Sock := TUSocket.Invalid;
  WriteLn('Host Make: ', Sock.MakeTCP());
  WriteLn('Host Bind: ', Sock.Bind(@SockAddr, SizeOf(SockAddr)));
  WriteLn('Host Listen: ', Sock.Listen(2));
  OtherSockAddrLen := SizeOf(OtherSockAddr);
  Client := TClient.Create(True);
  try
    Client.Start;
    SockOther := Sock.Accept(@OtherSockAddr, @OtherSockAddrLen);
    WriteLn('Host Accept: ', SockOther);
    WriteLn('Host Recv: ', SockOther.Recv);
    WriteLn('Host Send: ', SockOther.Send('Host Message'));
    Sleep(2000);
    WriteLn('Host Shutdown: ', SockOther.Shutdown());
    WriteLn('Host Close: ', SockOther.Close);
    WriteLn('Host Close Listen: ', Sock.Close);
  finally
    Client.WaitFor;
    FreeAndNil(Client);
  end;
end;

procedure TClient.Execute;
  var Sock: TUSocket;
  var SockAddr: TUSockAddr;
  var i, r: Int32;
begin
  Sleep(5000);
  Sock := TUSocket.Invalid;
  WriteLn('Client Make: ', Sock.MakeTCP());
  SockAddr := TUSockAddr.Default;
  SockAddr.sin_addr := TUInAddr.LocalhostN;
  SockAddr.sin_port := UNetHostToNetShort(5921);
  for i := 0 to 10 do
  begin
    r := Sock.Connect(@SockAddr, SizeOf(SockAddr));
    WriteLn('Client Connect: ', r);
    if r = 0 then Break;
    Sleep(1000);
  end;
  WriteLn('Client Send: ', Sock.Send('Client Message'));
  WriteLn('Client Recv: ', Sock.Recv);
  WriteLn('Client Shutdown: ', Sock.Shutdown());
  WriteLn('Client Close: ', Sock.Close);
end;

begin
  Run;
  WriteLn('Done');
  ReadLn;
end.

