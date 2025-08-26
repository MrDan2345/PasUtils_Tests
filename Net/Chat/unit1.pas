unit Unit1;

{$include PasUtilsMode.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CommonUtils, NetUtils, LCLType;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ButtonStartStop1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Timer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ButtonStartStop1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    type TListener = class (TThread)
      procedure Execute; override;
    end;
    type TPeer = TUNet.TBeacon.TPeer;
    type TPeers = TUNet.TBeacon.TPeerArray;
    type TReceivedMessage = record
      Addr: TUInAddr;
      Msg: String;
    end;
    var ReceivedMessages: array of TReceivedMessage;
    var ReceivedMessagesLock: TUCriticalSection;
    var NewMessages: Boolean;
    var Beacon: TUNet.TBeaconRef;
    var Peers: TPeers;
    var Sock: TUSocket;
    var Port: UInt16;
    var Listener: TListener;
    procedure SendMessage(const Msg: String);
    procedure OnPeerJoined(const Peer: TPeer);
    procedure OnPeerLeft(const Peer: TPeer);
    procedure OnTimer(Sender: TObject);
    procedure OnStart;
    procedure OnStop;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Sock := TUSocket.Invalid;
  Timer := TTimer.Create(Self);
  Timer.OnTimer := @OnTimer;
  Timer.Enabled := True;
  Beacon := TUNet.TBeacon.Create;
  Beacon.Ptr.Active := True;
end;

procedure TForm1.SendMessage(const Msg: String);
  var i, n: Int32;
  var SockAddr: TUSockAddr;
begin
  if not Sock.IsValid then Exit;
  SockAddr := TUSockAddr.Default;
  n := UMin(1408, Length(Msg));
  for i := 0 to High(Peers) do
  begin
    if not UStrIsNumber(Peers[i].Message) then Continue;
    SockAddr.sin_port := UNetHostToNetShort(StrToInt(Peers[i].Message));
    SockAddr.sin_addr := Peers[i].Addr;
    Sock.SendTo(@Msg[1], n, 0, @SockAddr, SizeOf(SockAddr));
    //Memo1.Append('Sent: ' + UNetNetAddrToStr(Peers[i].Addr) + ':' + Peers[i].Message);
  end;
  Memo1.Append(UNetHostName + ': ' + Msg);
end;

procedure TForm1.OnPeerJoined(const Peer: TPeer);
begin
  Memo1.Append(Peer.Name + ' has joined');
end;

procedure TForm1.OnPeerLeft(const Peer: TPeer);
begin
  Memo1.Append(Peer.Name + ' has left');
end;

procedure TForm1.OnTimer(Sender: TObject);
  function FindPeer(const PeerArray: TPeers; const Peer: TPeer): Int32;
    var i: Int32;
  begin
    for i := 0 to High(PeerArray) do
    if PeerArray[i] = Peer then
    begin
      Exit(i);
    end;
    Result := -1;
  end;
  var NewPeers: TPeers;
  var i, j: Int32;
begin
  NewPeers := Beacon.Ptr.Peers;
  for i := 0 to High(Peers) do
  if FindPeer(NewPeers, Peers[i]) = -1 then
  begin
    OnPeerLeft(Peers[i]);
  end;
  for i := 0 to High(NewPeers) do
  if FindPeer(Peers, NewPeers[i]) = -1 then
  begin
    OnPeerJoined(NewPeers[i]);
  end;
  Peers := NewPeers;
  if NewMessages then
  begin
    ReceivedMessagesLock.Enter;
    try
      for i := 0 to High(ReceivedMessages) do
      for j := 0 to High(Peers) do
      if Peers[j].Addr = ReceivedMessages[i].Addr then
      begin
        Memo1.Append(Peers[j].Name + ': ' + ReceivedMessages[j].Msg);
        Break;
      end;
      ReceivedMessages := nil;
      NewMessages := False;
    finally
      ReceivedMessagesLock.Leave;
    end;
  end;
end;

procedure TForm1.OnStart;
  var Addr: TUSockAddr;
  var i, r: Int32;
begin
  NewMessages := False;
  Sock := TUSocket.CreateUDP();
  Addr := TUSockAddr.Default;
  Port := 61390;
  for i := 0 to 9 do
  begin
    Addr.sin_port := UNetHostToNetShort(Port);
    r := Sock.Bind(@Addr, SizeOf(Addr));
    if r = 0 then Break;
    Inc(Port);
  end;
  if r <> 0 then
  begin
    Sock.Close;
    Sock := TUSocket.Invalid;
    Exit;
  end;
  //Memo1.Append('Started: ' + UNetNetAddrToStr(UNetLocalAddr) + ':' + IntToStr(Port));
  Listener := TListener.Create(True);
  Listener.Start;
  Beacon.Ptr.Message := IntToStr(Port);
  Beacon.Ptr.Enabled := True;
end;

procedure TForm1.OnStop;
  var i: Int32;
begin
  if not Sock.IsValid then Exit;
  Beacon.Ptr.Enabled := False;
  Listener.Terminate;
  Sock.Shutdown();
  Sock.Close;
  Sock := TUSocket.Invalid;
  Listener.WaitFor;
  Listener.Free;
  for i := 0 to High(Peers) do
  begin
    OnPeerLeft(Peers[i]);
  end;
  Peers := nil;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Sock.IsValid then OnStop;
end;

procedure TForm1.ButtonStartStop1Click(Sender: TObject);
begin
  if Sock.IsValid then
  begin
    ButtonStartStop1.Caption := 'Start';
    OnStop;
  end
  else
  begin
    ButtonStartStop1.Caption := 'Stop';
    OnStart;
  end;
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  if Length(Edit1.Text) = 0 then Exit;
  SendMessage(Edit1.Text);
  Edit1.Text := '';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Length(Edit1.Text) = 0 then Exit;
  SendMessage(Edit1.Text);
  Edit1.Text := '';
end;

procedure TForm1.TListener.Execute;
  const BufferSize = 1408;
  var Buffer: array[0..BufferSize - 1] of UInt8;
  var AddrFrom: TUSockAddr;
  var SockLen: TUSockLen;
  var Msg: TForm1.TReceivedMessage;
  var n: Int32;
begin
  while not Terminated do
  begin
    SockLen := SizeOf(AddrFrom);
    n := Form1.Sock.RecvFrom(@Buffer, BufferSize, 0, @AddrFrom, @SockLen);
    if n <= 0 then Continue;
    Form1.ReceivedMessagesLock.Enter;
    try
      Msg.Addr := AddrFrom.sin_addr;
      Msg.Msg := '';
      SetLength(Msg.Msg, n);
      Move(Buffer, Msg.Msg[1], n);
      specialize UArrAppend<TForm1.TReceivedMessage>(Form1.ReceivedMessages, Msg);
      Form1.NewMessages := True;
    finally
      Form1.ReceivedMessagesLock.Leave;
    end;
  end;
end;

end.

