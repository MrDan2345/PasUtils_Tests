unit Unit1;

{$include PasUtilsMode.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CommonUtils, NetUtils, LCLType;

type
  TChat = class (TURefClass)
  private
    type TListener = class (TThread)
    public
      var Chat: TChat;
      procedure Execute; override;
      procedure TerminatedSet; override;
    end;
    type TQuery = class (TThread)
    public
      var Chat: TChat;
      var Event: TUEvent;
      procedure Execute; override;
      procedure TerminatedSet; override;
    end;
    type TPacketDesc = (pd_ping, pd_pong, pd_query, pd_discover, pd_received, pd_msg);
    type TSimplePacket = packed record
      Marker: array[0..3] of AnsiChar;
      Desc: UInt8;
    end;
    const Marker = 'UNCP';
    const PacketPing: TSimplePacket = (Marker: Marker; Desc: UInt8(pd_ping));
    const PacketPong: TSimplePacket = (Marker: Marker; Desc: UInt8(pd_pong));
    var _Sock: TUSocket;
    var _Enabled: Boolean;
    var _PortRange: array[0..1] of UInt16;
    function PortMin: UInt16;
    function PortMax: UInt16;
    procedure Start;
    procedure Stop;
    procedure SetEnabled(const Value: Boolean);
    function GetPortRange(const Index: Int8): UInt16;
    procedure SetPortRange(const Index: Int8; const Value: UInt16);
    procedure PacketConfirmed(const Id, Index: UInt16);
    procedure ReceiveMessage(const InAddr: TUInAddr; const Msg: TUInt8Array);
  public
    property Sock: TUSocket read _Sock;
    property Enabled: Boolean read _Enabled write SetEnabled;
    property PortRange[const Index: Int8]: UInt16 read GetPortRange write SetPortRange;
    constructor Create;
    destructor Destroy; override;
  end;

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

function TChat.PortMin: UInt16;
begin
  Result := UMin(_PortRange[0], _PortRange[1]);
end;

function TChat.PortMax: UInt16;
begin
  Result := UMax(_PortRange[0], _PortRange[1]);
end;

procedure TChat.Start;
  var r: Int32;
  var p: UInt16;
  var Addr: TUSockAddr;
begin
  if _Sock.IsValid then Exit;
  Addr := TUSockAddr.Default;
  _Sock := TUSocket.CreateUDP();
  for p := PortMin to PortMax do
  begin
    Addr.sin_port := UNetHostToNetShort(p);
    r := _Sock.Bind(@Addr, SizeOf(Addr));
    if r = 0 then Break;
  end;
  if r <> 0 then
  begin
    _Sock.Close;
    _Sock := TUSocket.Invalid;
    Exit;
  end;

end;

procedure TChat.Stop;
begin
  if not _Sock.IsValid then Exit;
  _Sock.Shutdown();
  _Sock.Close;
  _Sock := TUSocket.Invalid;
end;

procedure TChat.SetEnabled(const Value: Boolean);
begin
  if _Enabled = Value then Exit;
  _Enabled := Value;
  if _Enabled then Start else Stop;
end;

function TChat.GetPortRange(const Index: Int8): UInt16;
begin
  Result := _PortRange[Index];
end;

procedure TChat.SetPortRange(const Index: Int8; const Value: UInt16);
begin
  _PortRange[Index] := Value;
end;

procedure TChat.PacketConfirmed(const Id, Index: UInt16);
begin

end;

procedure TChat.ReceiveMessage(const InAddr: TUInAddr; const Msg: TUInt8Array);
begin

end;

constructor TChat.Create;
begin
  _Sock := TUSocket.Invalid;
  _Enabled := False;
  _PortRange[0] := 61390;
  _PortRange[1] := 61390;
end;

destructor TChat.Destroy;
begin
  inherited Destroy;
end;

procedure TChat.TListener.Execute;
  const BufferSize = 1408;
  var Buffer: array[0..BufferSize - 1] of UInt16;
  var BufferPos: Int32;
  function ReadUInt8: UInt8;
  begin
    Result := PUint8(@Buffer[BufferPos])^;
    Inc(BufferPos, SizeOf(UInt8));
  end;
  function ReadUInt16: UInt16;
  begin
    Result := PUint16(@Buffer[BufferPos])^;
    Inc(BufferPos, SizeOf(UInt16));
  end;
  type TMultiChunk = record
    var Index: UInt16;
    var Data: TUInt8Array;
  end;
  type TMultiPacket = record
    var Id: UInt16;
    var Chunks: array of TMultiChunk;
  end;
  var MultiPackets: array of TMultiPacket;
  function FindMultiPacket(const Id: UInt16): Int32;
    var i: Int32;
  begin
    for i := 0 to High(MultiPackets) do
    if MultiPackets[i].Id = Id then
    begin
      Exit(i);
    end;
    Result := -1;
  end;
  function FindOrCreateMultiPacket(const Id: UInt16; const ChunkCount: UInt16): Int32;
    var i: Int32;
  begin
    Result := FindMultiPacket(Id);
    if Result > -1 then Exit;
    Result := Length(MultiPackets);
    SetLength(MultiPackets, Result + 1);
    with MultiPackets[Result] do
    begin
      Id := Id;
      SetLength(Chunks, ChunkCount);
      for i := 0 to ChunkCount - 1 do
      begin
        Chunks[i].Index := $ffff;
      end;
    end;
  end;
  function FindMultiChunk(const Chunks: array of TMultiChunk; const Index: UInt16): Int32;
    var i: Int32;
  begin
    for i := 0 to High(Chunks) do
    if Chunks[i].Index = Index then
    begin
      Exit(i);
    end;
    Result := -1;
  end;
  procedure AddMultiChunk(var MultiPacket: TMultiPacket; const Index: UInt16; const Data: TUInt8Array);
    var i, j: Int32;
  begin
    if FindMultiChunk(MultiPacket.Chunks, Index) > -1 then Exit;
    with MultiPacket do
    for i := 0 to High(Chunks) do
    begin
      if (Chunks[i].Index <> $ffff)
      and (Index < Chunks[j].Index) then
      begin
        Continue;
      end;
      if Chunks[i].Index > Index then
      for j := High(Chunks) downto i + 1 do
      begin
        Chunks[j].Index := Chunks[j - 1].Index;
        Chunks[j].Data := Chunks[j - 1].Data;
      end;
      Chunks[i].Index := Index;
      Chunks[i].Data := Data;
      Exit;
    end;
  end;
  function MultiPacketReady(const MultiPacket: TMultiPacket): Boolean;
    var i: Int32;
  begin
    for i := 0 to High(MultiPacket.Chunks) do
    if MultiPacket.Chunks[i].Index = $ffff then
    begin
      Exit(False);
    end;
    Result := True;
  end;
  function AssembleMultiPacket(const MultiPacket: TMultiPacket): TUInt8Array;
    var i, n: Int32;
  begin
    with MultiPacket do
    begin
      n := 0;
      for i := 0 to High(Chunks) do
      begin
        n += Length(Chunks[i].Data);
      end;
      Result := nil;
      SetLength(Result, n);
      n := 0;
      for i := 0 to High(Chunks) do
      begin
        Move(Chunks[i].Data[0], Result[n], Length(Chunks[i].Data));
        n += Length(Chunks[i].Data);
      end;
    end;
  end;
  type TPacketRcv = packed record
    Simple: TSimplePacket;
    Id: UInt16;
  end;
  type TPacketRcvMulti = packed record
    Rcv: TPacketRcv;
    Index: UInt16;
  end;
  var MarkerCheck: array[0..3] of AnsiChar;
  var AddrFrom: TUSockAddr;
  var SockLen: TUSockLen;
  var i, j, k, r: Int32;
  var Desc: TPacketDesc;
  var Id, PkCount, PkIndex: UInt16;
  var MultiPacket: Boolean;
  var Msg: TUInt8Array;
  var PacketRcvMulti: TPacketRcvMulti;
  var PacketRcv: TPacketRcv absolute PacketRcvMulti;
begin
  UClear(MarkerCheck, SizeOf(MarkerCheck));
  Msg := nil;
  MultiPackets := nil;
  while not Terminated do
  begin
    SockLen := SizeOf(AddrFrom);
    r := Chat.Sock.RecvFrom(@Buffer, BufferSize, 0, @AddrFrom, @SockLen);
    if r <= Length(Marker) + SizeOf(UInt16) then Continue;
    Move(Buffer, MarkerCheck, Length(Marker));
    if MarkerCheck <> Marker then Continue;
    BufferPos := Length(Marker);
    Desc := TPacketDesc(ReadUInt8);
    case Desc of
      pd_ping:
      begin
        Chat.Sock.SendTo(@PacketPong, SizeOf(PacketPong), 0, @AddrFrom, SizeOf(AddrFrom));
      end;
      pd_received:
      begin
        Id := ReadUInt16;
        if Id and $8000 > 0 then
        begin
          PkCount := ReadUInt16;
          PkIndex := ReadUInt16;
        end;
        Chat.PacketConfirmed(Id, PkIndex);
      end;
      pd_msg:
      begin
        Id := ReadUInt16;
        MultiPacket := Id and $8000 > 0;
        if MultiPacket then
        begin
          PkCount := ReadUInt16;
          PkIndex := ReadUInt16;
        end;
        SetLength(Msg, r - BufferPos);
        Move(Buffer[BufferPos], Msg[0], Length(Msg));
        PacketRcv.Simple.Marker := Marker;
        PacketRcv.Simple.Desc := UInt8(pd_received);
        PacketRcv.Id := Id;
        if MultiPacket then
        begin
          i := FindOrCreateMultiPacket(Id, PkCount);
          AddMultiChunk(MultiPackets[i], PkIndex, Msg);
          if MultiPacketReady(MultiPackets[i]) then
          begin
            Chat.ReceiveMessage(AddrFrom.sin_addr, AssembleMultiPacket(MultiPackets[i]));
            specialize UArrDelete<TMultiPacket>(MultiPackets, i);
          end;
          PacketRcvMulti.Index := PkIndex;
          Chat.Sock.SendTo(
            @PacketRcvMulti, SizeOf(PacketRcvMulti), 0, @AddrFrom, SizeOf(AddrFrom)
          );
        end
        else
        begin
          Chat.ReceiveMessage(AddrFrom.sin_addr, Msg);
          Chat.Sock.SendTo(
            @PacketRcv, SizeOf(PacketRcv), 0, @AddrFrom, SizeOf(AddrFrom)
          );
        end;
      end;
    end;
  end;
end;

procedure TChat.TListener.TerminatedSet;
begin
  inherited TerminatedSet;
end;

procedure TChat.TQuery.Execute;
  var PortMin, PortMax, p: UInt16;
  var Addr: TUSockAddr;
begin
  PortMin := Chat.PortMin;
  PortMax := Chat.PortMax;
  Addr := TUSockAddr.Default;
  Addr.sin_addr := UNetLocalAddr;
  Addr.sin_addr.Addr8[3] := $ff;
  while not Terminated do
  begin
    Chat.Sock.SendTo();
  end;
end;

procedure TChat.TQuery.TerminatedSet;
begin
  inherited TerminatedSet;
end;

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

