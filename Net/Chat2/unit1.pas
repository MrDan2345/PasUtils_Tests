unit Unit1;

{$include PasUtilsMode.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CommonUtils, NetUtils, LCLType;

type TChat = class (TURefClass)
public
  type TPeer = record
    Name: String;
    Addr: TUInAddr;
    Port: UInt16;
    TimeStamp: UInt64;
  end;
  type TPeerArray = array of TPeer;
private
  const BufferSizeUDP = 1408;
  type TPacketDesc = (pd_ping, pd_pong, pd_query, pd_discover, pd_received, pd_message);
  type TPacketBase = packed object
    var Marker: array[0..3] of AnsiChar;
    var Desc: UInt8;
  end;
  type TPacketMessage = packed object (TPacketBase)
    var Id: UInt16;
    var Count: UInt16;
    var Index: UInt16;
  end;
  type TPacketReceived = packed object (TPacketBase)
    var Id: UInt16;
    var Index: UInt16;
  end;
  type TPacketQuery = packed object (TPacketBase)
    var NameLength: UInt8;
  end;
  const Marker = 'UNCP';
  const PacketPing: TPacketBase = (Marker: Marker; Desc: UInt8(pd_ping));
  const PacketPong: TPacketBase = (Marker: Marker; Desc: UInt8(pd_pong));
  type TListener = class (TThread)
  public
    var Chat: TChat;
    procedure Execute; override;
    procedure TerminatedSet; override;
  end;
  type TQuery = class (TThread)
  private
    var Event: TUEvent;
  public
    var Chat: TChat;
    procedure Execute; override;
    procedure TerminatedSet; override;
  end;
  type TUpdate = class (TThread)
  private
    var Event: TUEvent;
  public
    var Chat: TChat;
    var UpdateRate: UInt32;
    var OnUpdate: TUProcedure;
    procedure Execute; override;
    procedure TerminatedSet; override;
  end;
  type TSendMessage = record
    var Id: UInt16;
    var Addr: TUInAddr;
    var Port: UInt16;
    var Chunks: array of record
      var Confirmed: Boolean;
      var Packet: TUInt8Array;
    end;
  end;
  type TSendMessageArray = array of TSendMessage;
  var _Name: String;
  var _Sock: TUSocket;
  var _Enabled: Boolean;
  var _PortRange: array[0..1] of UInt16;
  var _Peers: TPeerArray;
  var _PeersLock: TUCriticalSection;
  var _Listener: TListener;
  var _Query: TQuery;
  var _Update: TUpdate;
  var _MyAddr: TUInAddr;
  var _MyPort: UInt16;
  var _MsgId: UInt16;
  var _SendMessageQueue: TSendMessageArray;
  var QueryPacket: TUInt8Array;
  var DiscoverPacket: TUInt8Array;
  function PortMin: UInt16;
  function PortMax: UInt16;
  procedure Start;
  procedure Stop;
  procedure SetEnabled(const Value: Boolean);
  function GetPortRange(const Index: Int8): UInt16;
  procedure SetPortRange(const Index: Int8; const Value: UInt16);
  procedure PacketConfirmed(const Id, Index: UInt16);
  procedure ReceiveMessage(const InAddr: TUInAddr; const Msg: TUInt8Array);
  function FindPeerNL(const PeerAddr: TUInAddr; const PeerPort: UInt16): Int32;
  function AddPeer(
    const PeerName: String;
    const PeerAddr: TUInAddr;
    const PeerPort: UInt16
  ): Int32;
  function GetPeers: TPeerArray;
public
  property Name: String read _Name write _Name;
  property Sock: TUSocket read _Sock;
  property Enabled: Boolean read _Enabled write SetEnabled;
  property PortRange[const Index: Int8]: UInt16 read GetPortRange write SetPortRange;
  property Peers: TPeerArray read GetPeers;
  procedure Update;
  procedure Send(const Message: String);
  constructor Create;
  destructor Destroy; override;
end;
type TChatShared = specialize TUSharedRef<TChat>;

type TForm1 = class(TForm)
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
  var Chat: TChatShared;
  procedure OnTimer(Sender: TObject);
public

end;

var Form1: TForm1;

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
  type PPacketQuery = ^TPacketQuery;
  var r: Int32;
  var p: UInt16;
  var Addr: TUSockAddr;
  var NameLength: Int32;
  var Packet: PPacketQuery;
begin
  if _Sock.IsValid then Exit;
  _MsgId := 1;
  _SendMessageQueue := nil;
  _MyAddr := UNetLocalAddr;
  Addr := TUSockAddr.Default;
  _Sock := TUSocket.CreateUDP();
  _Sock.SetSockOpt(SO_BROADCAST, 1);
  for p := PortMin to PortMax do
  begin
    _MyPort := p;
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
  WriteLn('Listening: ', _MyPort);
  NameLength := UMin(Length(_Name), 40);
  SetLength(QueryPacket, SizeOf(TPacketQuery) + NameLength);
  Packet := @QueryPacket[0];
  Packet^.Marker := Marker;
  Packet^.Desc := UInt8(pd_query);
  Packet^.NameLength := NameLength;
  Move(_Name[1], QueryPacket[SizeOf(TPacketQuery)], NameLength);
  SetLength(DiscoverPacket, Length(QueryPacket));
  Move(QueryPacket[0], DiscoverPacket[0], Length(QueryPacket));
  Packet := @DiscoverPacket[0];
  Packet^.Desc := UInt8(pd_discover);
  _Listener := TListener.Create(True);
  _Listener.Chat := Self;
  _Query := TQuery.Create(True);
  _Query.Chat := Self;
  _Update := TUpdate.Create(True);
  _Update.Chat := Self;
  _Update.OnUpdate := @Update;
  _Update.UpdateRate := 100;
  _Listener.Start;
  _Query.Start;
  _Update.Start;
end;

procedure TChat.Stop;
begin
  if not _Sock.IsValid then Exit;
  _Query.Terminate;
  _Listener.Terminate;
  _Sock.Shutdown();
  _Sock.Close;
  _Sock := TUSocket.Invalid;
  _Query.WaitFor;
  _Listener.WaitFor;
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

function TChat.FindPeerNL(const PeerAddr: TUInAddr; const PeerPort: UInt16): Int32;
  var i: Int32;
begin
  for i := 0 to High(_Peers) do
  if (_Peers[i].Addr = PeerAddr)
  and (_Peers[i].Port = PeerPort) then
  begin
    Exit(i);
  end;
  Result := -1;
end;

function TChat.AddPeer(
  const PeerName: String;
  const PeerAddr: TUInAddr;
  const PeerPort: UInt16
): Int32;
begin
  _PeersLock.Enter;
  try
    Result := FindPeerNL(PeerAddr, PeerPort);
    if Result = -1 then
    begin
      Result := Length(_Peers);
      SetLength(_Peers, Result + 1);
      _Peers[Result].Addr := PeerAddr;
      _Peers[Result].Port := PeerPort;
    end;
    _Peers[Result].Name := PeerName;
    _Peers[Result].TimeStamp := GetTickCount64;
    WriteLn(PeerName);
  finally
    _PeersLock.Leave;
  end;
end;

function TChat.GetPeers: TPeerArray;
  var i: Int32;
begin
  Result := nil;
  _PeersLock.Enter;
  try
    SetLength(Result, Length(_Peers));
    for i := 0 to High(_Peers) do
    begin
      Result[i] := _Peers[i];
    end;
  finally
    _PeersLock.Leave;
  end;
end;

procedure TChat.Update;
begin

end;

procedure TChat.Send(const Message: String);
  type PPacketMessage = ^TPacketMessage;
  var Msg: PPacketMessage;
  var ChunkCount, ChunkSize, ChunkRem: Int32;
  var CurId: UInt16;
  var SendMessage: TSendMessage;
  var p, i, n, m: Int32;
  var Receivers: TPeerArray;
begin
  Receivers := GetPeers;
  CurId := _MsgId;
  Inc(_MsgId);
  if _MsgId = $ffff then _MsgId := 1;
  ChunkSize := BufferSizeUDP - SizeOf(TPacketMessage);
  ChunkCount := Length(Message) div ChunkSize;
  ChunkRem := Length(Message) mod ChunkSize;
  if ChunkRem > 0 then Inc(ChunkCount);
  for p := 0 to High(Receivers) do
  begin
    SendMessage.Id := CurId;
    SendMessage.Addr := Receivers[p].Addr;
    SendMessage.Port := Receivers[p].Port;
    SetLength(SendMessage.Chunks, ChunkCount);
    ChunkRem := Length(Message);
    m := 1;
    for i := 0 to ChunkCount - 1 do
    begin
      n := UMin(ChunkRem, ChunkSize);
      ChunkRem -= n;
      SendMessage.Chunks[i].Confirmed := False;
      SetLength(SendMessage.Chunks[i].Packet, SizeOf(TPacketMessage) + n);
      Msg := PPacketMessage(@SendMessage.Chunks[i].Packet[0]);
      Msg^.Id := CurId;
      Msg^.Marker := Marker;
      Msg^.Desc := UInt8(pd_message);
      Msg^.Count := UInt16(ChunkCount);
      Msg^.Index := UInt16(i);
      Move(Message[m], SendMessage.Chunks[i].Packet[SizeOf(TPacketMessage)], n);
      m += n;
    end;
  end;
end;

constructor TChat.Create;
begin
  _Name := UNetHostName;
  _Sock := TUSocket.Invalid;
  _Enabled := False;
  _PortRange[0] := 61390;
  _PortRange[1] := _PortRange[0];
end;

destructor TChat.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

procedure TChat.TListener.Execute;
  var Buffer: array[0..BufferSizeUDP - 1] of UInt8;
  type TMultiPacket = record
    var Id: UInt16;
    var Chunks: array of TUInt8Array;
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
  begin
    Result := FindMultiPacket(Id);
    if Result > -1 then Exit;
    Result := Length(MultiPackets);
    SetLength(MultiPackets, Result + 1);
    with MultiPackets[Result] do
    begin
      Id := Id;
      SetLength(Chunks, ChunkCount);
    end;
  end;
  procedure AddMultiChunk(var MultiPacket: TMultiPacket; const Index: UInt16; const Data: TUInt8Array);
  begin
    if Length(MultiPacket.Chunks[Index]) > 0 then Exit;
    MultiPacket.Chunks[Index] := Data;
  end;
  function MultiPacketReady(const MultiPacket: TMultiPacket): Boolean;
    var i: Int32;
  begin
    for i := 0 to High(MultiPacket.Chunks) do
    if Length(MultiPacket.Chunks[i]) = 0 then
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
        n += Length(Chunks[i]);
      end;
      Result := nil;
      SetLength(Result, n);
      n := 0;
      for i := 0 to High(Chunks) do
      begin
        Move(Chunks[i][0], Result[n], Length(Chunks[i]));
        n += Length(Chunks[i]);
      end;
    end;
  end;
  var MarkerCheck: array[0..3] of AnsiChar;
  var AddrFrom: TUSockAddr;
  var SockLen: TUSockLen;
  var i, r: Int32;
  var PacketBase: TPacketBase absolute Buffer;
  var PacketMessage: TPacketMessage absolute Buffer;
  var PacketReceived: TPacketReceived absolute Buffer;
  var PacketQuery: TPacketQuery absolute Buffer;
  var PkCount, PkIndex: UInt16;
  var Msg: TUInt8Array;
  var PacketRcv: TPacketReceived;
  var PeerName: String;
begin
  UClear(MarkerCheck, SizeOf(MarkerCheck));
  Msg := nil;
  MultiPackets := nil;
  PeerName := '';
  PacketRcv.Marker := Marker;
  PacketRcv.Desc := UInt8(pd_received);
  while not Terminated do
  begin
    SockLen := SizeOf(AddrFrom);
    r := Chat.Sock.RecvFrom(@Buffer, BufferSizeUDP, 0, @AddrFrom, @SockLen);
    if r <= SizeOf(PacketBase) then Continue;
    if (AddrFrom.sin_addr = Chat._MyAddr) and (NtoHs(AddrFrom.sin_port) = Chat._MyPort) then Continue;
    if PacketBase.Marker <> Marker then Continue;
    WriteLn('Receivd: ', UNetNetAddrToStr(AddrFrom.sin_addr), ':', NtoHs(AddrFrom.sin_port));
    case TPacketDesc(PacketBase.Desc) of
      pd_ping:
      begin
        Chat.Sock.SendTo(@PacketPong, SizeOf(PacketPong), 0, @AddrFrom, SizeOf(AddrFrom));
      end;
      pd_query:
      begin
        if r - SizeOf(PacketQuery) <> PacketQuery.NameLength then Continue;
        Chat.Sock.SendTo(@Chat.DiscoverPacket[0], Length(Chat.DiscoverPacket), 0, @AddrFrom, SizeOf(AddrFrom));
        SetLength(PeerName, PacketQuery.NameLength);
        Move(Buffer[SizeOf(PacketQuery)], PeerName[1], PacketQuery.NameLength);
        Chat.AddPeer(PeerName, AddrFrom.sin_addr, UNetNetToHostShort(AddrFrom.sin_port));
      end;
      pd_discover:
      begin
        if r - SizeOf(PacketQuery) <> PacketQuery.NameLength then Continue;
        SetLength(PeerName, PacketQuery.NameLength);
        Move(Buffer[SizeOf(PacketQuery)], PeerName[1], PacketQuery.NameLength);
        Chat.AddPeer(PeerName, AddrFrom.sin_addr, UNetNetToHostShort(AddrFrom.sin_port));
      end;
      pd_received:
      begin
        Chat.PacketConfirmed(PacketReceived.Id, PacketReceived.Index);
      end;
      pd_message:
      begin
        PkCount := PacketMessage.Count;
        PkIndex := PacketMessage.Index;
        SetLength(Msg, r - SizeOf(PacketMessage));
        Move(Buffer[SizeOf(PacketMessage)], Msg[0], Length(Msg));
        PacketRcv.Id := PacketMessage.Id;
        PacketRcv.Index := PkIndex;
        i := FindOrCreateMultiPacket(PacketMessage.Id, PkCount);
        AddMultiChunk(MultiPackets[i], PkIndex, Msg);
        if MultiPacketReady(MultiPackets[i]) then
        begin
          Chat.ReceiveMessage(AddrFrom.sin_addr, AssembleMultiPacket(MultiPackets[i]));
          specialize UArrDelete<TMultiPacket>(MultiPackets, i);
        end;
        Chat.Sock.SendTo(
          @PacketRcv, SizeOf(TPacketReceived), 0, @AddrFrom, SizeOf(AddrFrom)
        );
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
  Event.Unsignal;
  while not Terminated do
  begin
    for p := PortMin to PortMax do
    begin
      Addr.sin_port := UNetHostToNetShort(p);
      Chat.Sock.SendTo(
        @Chat.QueryPacket[0], Length(Chat.QueryPacket),
        0, @Addr, SizeOf(Addr)
      );
      WriteLn('Broadcast: ', UNetNetAddrToStr(Addr.sin_addr) + ':', p);
    end;
    Event.WaitFor(5000);
  end;
end;

procedure TChat.TQuery.TerminatedSet;
begin
  inherited TerminatedSet;
  Event.Signal;
end;

procedure TChat.TUpdate.Execute;
begin
  Event.Unsignal;
  while not Terminated do
  begin
    if Assigned(OnUpdate) then OnUpdate();
    Event.WaitFor(UpdateRate);
  end;
end;

procedure TChat.TUpdate.TerminatedSet;
begin
  inherited TerminatedSet;
  Event.Signal;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chat := TChat.Create;
  Chat.Ptr.PortRange[1] := Chat.Ptr.PortRange[0] + 1;
  Timer := TTimer.Create(Self);
  Timer.OnTimer := @OnTimer;
  Timer.Enabled := True;
end;

procedure TForm1.OnTimer(Sender: TObject);
begin

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Chat.Ptr.Enabled := False;
end;

procedure TForm1.ButtonStartStop1Click(Sender: TObject);
begin
  if Chat.Ptr.Enabled then
  begin
    Chat.Ptr.Enabled := False;
    ButtonStartStop1.Caption := 'Start';
  end
  else
  begin
    Chat.Ptr.Enabled := True;
    ButtonStartStop1.Caption := 'Stop';
  end;
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  if Length(Edit1.Text) = 0 then Exit;
  Edit1.Text := '';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Length(Edit1.Text) = 0 then Exit;
  Edit1.Text := '';
end;

end.

