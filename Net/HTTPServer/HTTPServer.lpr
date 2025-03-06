program HTTPServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  NetUtils,
  WinSock2,
  Sockets;

type TInput = class (TThread)
public
  type TOnInputProc = procedure (const Inlut: String; out Terminate: Boolean) of Object;
private
  var _OnInput: TOnInputProc;
public
  property OnInput: TOnInputProc read _OnInput write _OnInput;
  procedure Execute; override;
end;

type TServer = class (TURefClass)
private
  type TMaintenanace = class (TThread)
  public
    var Server: TServer;
    var Event: TUEvent;
    procedure Execute; override;
    procedure TerminatedSet; override;
  end;
  type TRequestHandler = class (TThread)
  public
    var SockClient: TUSocket;
    var Addr: TUSockAddr;
    var ServeDir: String;
    procedure Execute; override;
    procedure TerminatedSet; override;
  end;
  var _Maintenance: TMaintenanace;
  var _ServeDir: String;
  var _SockListen: TUSocket;
  var _LockRequests: TUCriticalSection;
  var _Requests: array of TRequestHandler;
  procedure HandleRequest(const SockClient: TUSocket; const Addr: TUSockAddr);
  procedure Maintenance;
public
  constructor Create;
  destructor Destroy; override;
  procedure OnInput(const Input: String; out Terminate: Boolean);
  procedure Start;
  procedure Stop;
  procedure Process;
end;
type TServerShared = specialize TUSharedRef<TServer>;

procedure TServer.TMaintenanace.Execute;
begin
  Event.Unsignal;
  Event.WaitFor(1000);
  while not Terminated do
  begin
    Server.Maintenance;
    Event.WaitFor(2000);
  end;
end;

procedure TServer.TMaintenanace.TerminatedSet;
begin
  inherited TerminatedSet;
  Event.Signal;
end;

procedure TServer.TRequestHandler.Execute;
  function ExtractHeader(const Request: String): TUStrArray;
    var i: Int32;
    var s: String;
  begin
    Result := nil;
    i := Request.IndexOf(#$D#$A#$D#$A);
    if i = -1 then Exit;
    s := Request.Substring(0, i);
    Result := UStrExplode(Request, #$D#$A, False);
  end;
  function ProcessRequest(const Request: TUStrArray): Int32;
    function FindHeader(const Name: String): String;
      var Arr: TUStrArray;
      var i: Int32;
    begin
      for i := 0 to High(Request) do
      begin
        Arr := UStrExplode(Request[i], ':');
        if Length(Arr) < 2 then Continue;
        if LowerCase(Arr[0]) = LowerCase(Name) then Exit(Arr[1].TrimLeft);
      end;
      Result := '';
    end;
    function CheckAcceptContent(const ContentToCheck: array of String): Boolean;
      var Accept: String;
      var AcceptContent: TUStrArray;
      var i, j: Int32;
    begin
      Accept := FindHeader('Accept');
      if Length(Accept) = 0 then Exit(False);
      AcceptContent := UStrExplode(Accept, ';', False);
      if Length(AcceptContent) = 0 then Exit(False);
      AcceptContent := UStrExplode(AcceptContent[0], ',', False);
      for i := 0 to High(AcceptContent) do
      for j := 0 to High(ContentToCheck) do
      if LowerCase(AcceptContent[i]) = LowerCase(ContentToCheck[j]) then
      begin
        Exit(True);
      end;
      Result := False;
    end;
    var Method: TUStrArray;
    var ServeFile: String;
    var Header: String;
    var Response: String;
  begin
    Method := UStrExplode(Request[0], ' ', True);
    if Length(Method) < 2 then Exit(406);
    if LowerCase(Method[0]) <> 'get' then Exit(406);
    if Method[1] = '/' then Method[1] := '/index.html';
    ServeFile := ServeDir + Method[1];
    if not CheckAcceptContent(['text/html']) then Exit(415);
    if not FileExists(ServeFile) then Exit(404);
    Result := 200;
    Response := '<!doctype html>'#$D#$A + UFileToStr(ServeFile);
    Header := 'HTTP/1.1 200 OK'#$D#$A;
    Header += 'Content-Type: text/html; charset=utf-8'#$D#$A;
    Header += 'Content-Length: ' + IntToStr(Length(Response)) + #$D#$A;
    Header += 'Connection: close'#$D#$A;
    Header += #$D#$A;
    Response := Header + Response;
    SockClient.Send(@Response[1], Length(Response), 0);
  end;
  procedure SendError406;
    var Response: String;
  begin
    Response := 'HTTP/1.1 406 Not Acceptable'#$D#$A;
    Response += 'Connection: close'#$D#$A;
    Response += #$D#$A;
    SockClient.Send(@Response[1], Length(Response), 0);
  end;
  procedure SendError415;
    var Response: String;
  begin
    Response := 'HTTP/1.1 415 Unsupported Media Type'#$D#$A;
    Response += 'Connection: close'#$D#$A;
    Response += #$D#$A;
    SockClient.Send(@Response[1], Length(Response), 0);
  end;
  procedure SendError404;
    var Header: String;
    var Response: String;
  begin
    Response := '<!DOCTYPE html>'#$D#$A;
    Response += '<html>'#$D#$A;
    Response += '  <head>'#$D#$A;
    Response += '    <title>404 Not Found</title>'#$D#$A;
    Response += '  </head>'#$D#$A;
    Response += '  <body>'#$D#$A;
    Response += '    <h1>Not Found</h1>'#$D#$A;
    Response += '    <p>The requested URL was not found on this server.</p>'#$D#$A;
    Response += '  </body>'#$D#$A;
    Response += '</html>';
    Header := 'HTTP/1.1 404 Not Found'#$D#$A;
    Header += 'Content-Type: text/html; charset=UTF-8'#$D#$A;
    Header += 'Content-Length: ' + IntToStr(Length(Response)) + #$D#$A;
    Header += 'Connection: close'#$D#$A;
    Header += #$D#$A;
    Response := Header + Response;
    SockClient.Send(@Response[1], Length(Response), 0);
  end;
  const BufferSize = 8 * 1024;
  var Buffer: array[0..BufferSize - 1] of Byte;
  var Request: String;
  var r, i: Int32;
  var Header: TUStrArray;
begin
  Request := '';
  //SockClient.SetSockOpt(SO_RCVTIMEO, 2000);
  r := SockClient.Recv(@Buffer, SizeOf(Buffer), 0);
  try
    if r <= 0 then
    begin
      WriteLn('Error receiving request from ',
        UNetNetAddrToStr(Addr.sin_addr), ':',
        NToHs(Addr.sin_port)
      );
      Exit;
    end;
    SetLength(Request, r);
    Move(Buffer, Request[1], r);
    WriteLn(Request);
    Header := ExtractHeader(Request);
    if Length(Header) = 0 then
    begin
      SendError406;
      Exit;
    end;
    r := ProcessRequest(Header);
    case r of
      200: {success};
      404: SendError404;
      406: SendError406;
      415: SendError415;
      else Exit;
    end;
  finally
    SockClient.Shutdown();
    SockClient.Close;
  end;
end;

procedure TServer.TRequestHandler.TerminatedSet;
begin
  inherited TerminatedSet;
  if not SockClient.IsValid then Exit;
  SockClient.Shutdown();
  SockClient.Close;
end;

procedure TInput.Execute;
  var Command: String;
  var Params: TUStrArray;
  var t: Boolean;
begin
  while not Terminated do
  begin
    Command := '';
    ReadLn(Command);
    if Length(Command) = 0 then Continue;
    Command := LowerCase(Command);
    Params := UStrExplode(Command, ' ');
    if Length(Params) = 0 then Continue;
    Command := Params[0];
    if Assigned(_OnInput) then
    begin
      _OnInput(Command, t);
      if t then Terminate;
    end
    else
    begin
      Terminate;
    end;
  end;
end;

procedure TServer.HandleRequest(const SockClient: TUSocket; const Addr: TUSockAddr);
  var Handler: TRequestHandler;
begin
  Handler := TRequestHandler.Create(True);
  try
    Handler.SockClient := SockClient;
    Handler.Addr := Addr;
    Handler.ServeDir := _ServeDir;
    _LockRequests.Enter;
    try
      specialize UArrAppend<TRequestHandler>(_Requests, Handler);
    finally
      _LockRequests.Leave;
    end;
  finally
    Handler.Start;
  end;
end;

procedure TServer.Maintenance;
  var i: Int32;
begin
  _LockRequests.Enter;
  try
    for i := High(_Requests) downto 0 do
    begin
      if not _Requests[i].Finished then Continue;
      _Requests[i].Free;
      specialize UArrDelete<TRequestHandler>(_Requests, i);
    end;
  finally
    _LockRequests.Leave;
  end;
end;

constructor TServer.Create;
  var Args: TUArguments;
  function GetServeDir: String;
  begin
    try
      Result := Args.ParamValue('dir');
      if (Length(Result) > 0) then
      begin
        if DirectoryExists(Result) then Exit;
        WriteLn('Warning: invalid directory: ', Result);
      end;
      Result := ExpandFileName(ExtractFileDir(ParamStr(0)));
      if DirectoryExists(Result + DirectorySeparator + 'http') then
      begin
        Result += DirectorySeparator + 'http';
        Exit;
      end;
    finally
      Result := Result.TrimRight(['/', '\']);
    end;
  end;
begin
  _ServeDir := GetServeDir;
  WriteLn('Serving directory: ', _ServeDir);
end;

destructor TServer.Destroy;
begin
  inherited Destroy;
end;

procedure TServer.OnInput(const Input: String; out Terminate: Boolean);
  var Command: String;
begin
  Terminate := False;
  Command := LowerCase(Input);
  if (Command = 'exit')
  or (Command = 'quit') then
  begin
    Terminate := True;
  end;
  if Terminate then Stop;
end;

procedure TServer.Start;
  function TryBind: Boolean;
    var ListenAddr: TUSockAddr;
    var r, n: Int32;
  begin
    ListenAddr := TUSockAddr.Default;
    ListenAddr.sin_addr := TUInAddr.Any;
    ListenAddr.sin_port := HToNs(80);
    n := 0;
    repeat
      r := _SockListen.Bind(@ListenAddr, SizeOf(ListenAddr));
      if r = 0 then Break;
      Inc(n);
      if n >= 5 then
      begin
        WriteLn('Failed to bind to port 80, aborting.');
        Exit(False);
      end;
      WriteLn('Failed to bind to port 80 (try=', n, ').');
      Sleep(2000);
    until r = 0;
    Result := True;
  end;
begin
  _Maintenance := TMaintenanace.Create(True);
  _Maintenance.Server := Self;
  _Maintenance.Start;
  try
    _SockListen := TUSocket.CreateTCP();
    try
      if not TryBind then Exit;
      if _SockListen.Listen(10) <> 0 then
      begin
        WriteLn('Failed to start listen server.');
        Exit;
      end;
      Process;
    finally
      Stop;
    end;
  finally
    _Maintenance.Terminate;
    _Maintenance.WaitFor;
    _Maintenance.Free;
  end;
end;

procedure TServer.Stop;
begin
  if not _SockListen.IsValid then Exit;
  _SockListen.Shutdown;
  _SockListen.Close;
end;

procedure TServer.Process;
  var SockClient: TUSocket;
  var ClientAddr: TUSockAddr;
  var ClinetAddrLen: TUSockLen;
begin
  while _SockListen.IsValid do
  begin
    ClinetAddrLen := SizeOf(ClientAddr);
    SockClient := _SockListen.Accept(@ClientAddr, @ClinetAddrLen);
    if not SockClient.IsValid then Continue;
    HandleRequest(SockClient, ClientAddr);
  end;
end;

procedure Run;
  var Server: TServerShared;
  var Input: TInput;
begin
  Server := TServer.Create;
  Input := TInput.Create(True);
  try
    Input.OnInput := @Server.Ptr.OnInput;
    Input.Start;
    Server.Ptr.Start;
  finally
    Input.Free;
  end;
end;

begin
  WriteLn('PasUtils http server.');
  WriteLn('Parameters:');
  WriteLn('  -dir=(path)           specify a directory to serve.');
  WriteLn('Type `exit` or `quit` to stop serving.');
  Run;
  WriteLn('Done.');
end.

