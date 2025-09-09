unit Unit1;

{$include PasUtilsMode.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CommonUtils, NetUtils, LCLType;

type TChat = class (TURefClass)
public
  type TPeerId = record
    var Name: String;
    var Addr: TUInAddr;
    var Port: UInt16;
    class operator = (const a, b: TPeerId): Boolean;
    function SockAddr: TUSockAddr;
    function CmpAddr(const PeerAddr: TUInAddr; const PeerPort: UInt16): Boolean;
  end;
  type TPeerIdArray = array of TPeerId;
  type TPeerEvent = procedure (const Peer: TPeerId) of object;
  type TMessageEvent = procedure (const Peer: TPeerId; const Message: String) of object;
private
  const BufferSizeUDP = 1408;
  type TPacketDesc = (pd_ping, pd_pong, pd_close, pd_query, pd_discover, pd_received, pd_message);
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
  const PacketClose: TPacketBase = (Marker: Marker; Desc: UInt8(pd_close));
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
  type TMessageReceiver = record
    var Confirmed: Boolean;
    var Addr: TUInAddr;
    var Port: UInt16;
  end;
  type TSendMessageChunk = record
    var Receivers: array of TMessageReceiver;
    var Packet: TUInt8Array;
  end;
  type TSendMessage = record
    var Id: UInt16;
    var Timestamp: UInt64;
    var Chunks: array of TSendMessageChunk;
  end;
  type TSendMessageArray = array of TSendMessage;
  type TPeerMessage = record
    var Peer: TPeerId;
    var Message: String;
  end;
  type TMessageChunk = record
    var Success: Boolean;
    var Data: TUInt8Array;
  end;
  type TMessageSend = record
    var Id: UInt16;
    var TimeStamp: UInt64;
    var Chunks: array of TMessageChunk;
  end;
  type TMessageRecv = record
    var Id: UInt16;
    var Chunks: array of TMessageChunk;
  end;
  type TPeer = record
    var Id: TPeerId;
    var TimeStamp: UInt64;
    var QueueSend: array of TMessageSend;
    var QueneRecv: array of TMessageRecv;
  end;
  type TPeerArray = array of TPeer;
  var _Name: String;
  var _Sock: TUSocket;
  var _Enabled: Boolean;
  var _PortRange: array[0..1] of UInt16;
  var _Peers: TPeerArray;
  var _PeersLock: TUCriticalSection;
  var _NotifiedPeers: TPeerIdArray;
  var _Messages: array of TPeerMessage;
  var _MessagesLock: TUCriticalSection;
  var _Listener: TListener;
  var _Query: TQuery;
  var _Update: TUpdate;
  var _MyAddr: TUInAddr;
  var _MyPort: UInt16;
  var _MsgId: UInt16;
  var _SendMessageQueue: TSendMessageArray;
  var _OnPeerJoined: TPeerEvent;
  var _OnPeerLeft: TPeerEvent;
  var _OnMessage: TMessageEvent;
  var QueryPacket: TUInt8Array;
  var DiscoverPacket: TUInt8Array;
  function PortMin: UInt16;
  function PortMax: UInt16;
  procedure Start;
  procedure Stop;
  procedure SetEnabled(const Value: Boolean);
  function GetPortRange(const Index: Int8): UInt16;
  procedure SetPortRange(const Index: Int8; const Value: UInt16);
  procedure PacketConfirmed(const InAddr: TUInAddr; const Port, Id, Index: UInt16);
  procedure ReceiveMessage(const PeerAddr: TUInAddr; const PeerPort: UInt16; const Msg: TUInt8Array);
  procedure CleanupQueue;
  function FindPeerNL(const PeerAddr: TUInAddr; const PeerPort: UInt16): Int32;
  function AddPeer(
    const PeerName: String;
    const PeerAddr: TUInAddr;
    const PeerPort: UInt16
  ): Int32;
  procedure RemovePeer(
    const PeerAddr: TUInAddr;
    const PeerPort: UInt16
  );
  procedure RemoveOldPeers;
  procedure NotifyPeers;
  function GetPeers: TPeerIdArray;
  procedure ProcessMessages;
  procedure ProcessSendMessage(var Msg: TSendMessage);
  procedure PeerAdded(const Peer: TPeerId);
  procedure PeerRemoved(const Peer: TPeerId);
  procedure Update;
public
  property Name: String read _Name write _Name;
  property Sock: TUSocket read _Sock;
  property Enabled: Boolean read _Enabled write SetEnabled;
  property PortRange[const Index: Int8]: UInt16 read GetPortRange write SetPortRange;
  property Peers: TPeerIdArray read GetPeers;
  property OnPeerJoined: TPeerEvent read _OnPeerJoined write _OnPeerJoined;
  property OnPeerLeft: TPeerEvent read _OnPeerLeft write _OnPeerLeft;
  property OnMessge: TMessageEvent read _OnMessage write _OnMessage;
  procedure Send(const Message: String);
  constructor Create;
  destructor Destroy; override;
  function DebugMessageQueue: Int32;
end;
type TChatShared = specialize TUSharedRef<TChat>;

type TForm1 = class(TForm)
  Button1: TButton;
  Button2: TButton;
  ButtonStartStop1: TButton;
  Edit1: TEdit;
  LabelDebugMsgQueue1: TLabel;
  Memo1: TMemo;
  Timer: TTimer;
  procedure Button1Click(Sender: TObject);
  procedure Button2Click(Sender: TObject);
  procedure ButtonStartStop1Click(Sender: TObject);
  procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
private
  var Chat: TChatShared;
  procedure OnTimer(Sender: TObject);
  procedure OnPeerJoined(const Peer: TChat.TPeerId);
  procedure OnPeerLeft(const Peer: TChat.TPeerId);
  procedure OnMessage(const Peer: TChat.TPeerId; const Message: String);
  procedure SendMessage(const Msg: String);
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
  _Peers := nil;
  _NotifiedPeers := nil;
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
  _Update := TUpdate.Create(True);
  _Update.Chat := Self;
  _Update.OnUpdate := @Update;
  _Update.UpdateRate := 100;
  _Listener := TListener.Create(True);
  _Listener.Chat := Self;
  _Query := TQuery.Create(True);
  _Query.Chat := Self;
  _Update.Start;
  _Listener.Start;
  _Query.Start;
end;

procedure TChat.Stop;
  var i: Int32;
  var SockAddr: TUSockAddr;
begin
  if not _Sock.IsValid then Exit;
  _Query.Terminate;
  _Listener.Terminate;
  _Update.Terminate;
  _Query.WaitFor;
  for i := 0 to High(_Peers) do
  begin
    SockAddr := _Peers[i].Id.SockAddr;
    _Sock.SendTo(@PacketClose, SizeOf(PacketClose), 0, @SockAddr, SizeOf(SockAddr));
  end;
  _Sock.Shutdown();
  _Sock.Close;
  _Sock := TUSocket.Invalid;
  _Listener.WaitFor;
  _Update.WaitFor;
  _Peers := nil;
  Update;
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

procedure TChat.PacketConfirmed(const InAddr: TUInAddr; const Port, Id, Index: UInt16);
  var i, p: Int32;
begin
  for i := 0 to High(_SendMessageQueue) do
  begin
    if _SendMessageQueue[i].Id <> Id then Continue;
    with _SendMessageQueue[i].Chunks[Index] do
    for p := 0 to High(Receivers) do
    begin
      if Receivers[p].Addr <> InAddr then Continue;
      if Receivers[p].Port <> Port then Continue;
      Receivers[p].Confirmed := True;
    end;
  end;
  CleanupQueue;
end;

procedure TChat.ReceiveMessage(
  const PeerAddr: TUInAddr;
  const PeerPort: UInt16;
  const Msg: TUInt8Array
);
  var p: Int32;
  var MsgStr: String;
  var PeerMessage: TPeerMessage;
begin
  if Length(Msg) = 0 then Exit;
  _PeersLock.Enter;
  try
    p := FindPeerNL(PeerAddr, PeerPort);
    if p = -1 then Exit;
    PeerMessage.Peer := _Peers[p].Id;
  finally
    _PeersLock.Leave;
  end;
  MsgStr := '';
  SetLength(MsgStr, Length(Msg));
  Move(Msg[0], MsgStr[1], Length(Msg));
  PeerMessage.Message := MsgStr;
  WriteLn('[', UNetNetAddrToStr(PeerAddr), ']: ', MsgStr);
  _MessagesLock.Enter;
  try
    specialize UArrAppend<TPeerMessage>(_Messages, PeerMessage);
  finally
    _MessagesLock.Leave;
  end;
end;

procedure TChat.CleanupQueue;
  var i, j, p: Int32;
  var Rem: Boolean;
begin
  for i := High(_SendMessageQueue) downto 0 do
  begin
    with _SendMessageQueue[i] do
    for j := High(Chunks) downto 0 do
    begin
      Rem := True;
      with Chunks[j] do
      for p := High(Receivers) downto 0 do
      if not Receivers[p].Confirmed then
      begin
        Rem := False;
        Break;
      end;
      if not Rem then Continue;
      specialize UArrDelete<TSendMessageChunk>(Chunks, j);
    end;
    if Length(_SendMessageQueue[i].Chunks) > 0 then Continue;
    specialize UArrDelete<TSendMessage>(_SendMessageQueue, i);
  end;
end;

function TChat.FindPeerNL(const PeerAddr: TUInAddr; const PeerPort: UInt16): Int32;
  var i: Int32;
begin
  for i := 0 to High(_Peers) do
  if _Peers[i].Id.CmpAddr(PeerAddr, PeerPort) then
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
      _Peers[Result].Id.Addr := PeerAddr;
      _Peers[Result].Id.Port := PeerPort;
    end;
    _Peers[Result].Id.Name := PeerName;
    _Peers[Result].TimeStamp := GetTickCount64;
    //WriteLn(PeerName);
  finally
    _PeersLock.Leave;
  end;
end;

procedure TChat.RemovePeer(const PeerAddr: TUInAddr; const PeerPort: UInt16);
  var i: Int32;
begin
  _PeersLock.Enter;
  try
    i := FindPeerNL(PeerAddr, PeerPort);
    if i = -1 then Exit;
    specialize UArrDelete<TPeer>(_Peers, i);
  finally
    _PeersLock.Leave;
  end;
end;

procedure TChat.RemoveOldPeers;
  var i: Int32;
  var t: UInt64;
begin
  t := GetTickCount64;
  _PeersLock.Enter;
  try
    for i := High(_Peers) downto 0 do
    if t - _Peers[i].TimeStamp > 15000 then
    begin
      specialize UArrDelete<TPeer>(_Peers, i);
    end;
  finally
    _PeersLock.Leave;
  end;
end;

procedure TChat.NotifyPeers;
  function FindPeer(const PeerArray: TPeerIdArray; const Peer: TPeerId): Int32;
    var i: Int32;
  begin
    for i := 0 to High(PeerArray) do
    if PeerArray[i] = Peer then
    begin
      Exit(i);
    end;
    Result := -1;
  end;
  var CurPeers: TPeerIdArray;
  var i: Int32;
begin
  CurPeers := GetPeers;
  for i := 0 to High(CurPeers) do
  if FindPeer(_NotifiedPeers, CurPeers[i]) = -1 then
  begin
    PeerAdded(CurPeers[i]);
    specialize UArrAppend<TPeerId>(_NotifiedPeers, CurPeers[i]);
  end;
  for i := High(_NotifiedPeers) downto 0 do
  if FindPeer(CurPeers, _NotifiedPeers[i]) = -1 then
  begin
    PeerRemoved(_NotifiedPeers[i]);
    specialize UArrDelete<TPeerId>(_NotifiedPeers, i);
  end;
end;

function TChat.GetPeers: TPeerIdArray;
  var i: Int32;
begin
  Result := nil;
  _PeersLock.Enter;
  try
    SetLength(Result, Length(_Peers));
    for i := 0 to High(_Peers) do
    begin
      Result[i] := _Peers[i].Id;
    end;
  finally
    _PeersLock.Leave;
  end;
end;

procedure TChat.ProcessMessages;
  var SockAddr: TUSockAddr;
  var p, m, c: Int32;
  var t: UInt64;
begin
  t := GetTickCount64;
  _PeersLock.Enter;
  try
    for p := 0 to High(_Peers) do
    begin
      SockAddr := _Peers[p].Id.SockAddr;
      with _Peers[p] do
      for m := 0 to High(QueueSend) do
      with QueueSend[m] do
      if t - TimeStamp > 2000 then
      begin
        TimeStamp := t;
        for c := 0 to High(Chunks) do
        if not Chunks[c].Success then
        begin
          _Sock.SendTo(
            @Chunks[c].Data[0], Length(Chunks[c].Data),
            0, @SockAddr, SizeOf(SockAddr)
          );
        end;
      end;
    end;
  finally
    _PeersLock.Leave;
  end;
end;

procedure TChat.ProcessSendMessage(var Msg: TSendMessage);
  var i, p: Int32;
  var SockAddr: TUSockAddr;
begin
  SockAddr := TUSockAddr.Default;
  Msg.Timestamp := GetTickCount64;
  for i := 0 to High(Msg.Chunks) do
  for p := 0 to High(Msg.Chunks[i].Receivers) do
  begin
    if Msg.Chunks[i].Receivers[p].Confirmed then Continue;
    SockAddr.sin_addr := Msg.Chunks[i].Receivers[p].Addr;
    SockAddr.sin_port := HtoNs(Msg.Chunks[i].Receivers[p].Port);
    _Sock.SendTo(
      @Msg.Chunks[i].Packet[0], Length(Msg.Chunks[i].Packet),
      0, @SockAddr, SizeOf(SockAddr)
    );
  end;
end;

procedure TChat.PeerAdded(const Peer: TPeerId);
begin
  WriteLn('Peer added: ', Peer.Name);
  if Assigned(_OnPeerJoined) then _OnPeerJoined(Peer);
end;

procedure TChat.PeerRemoved(const Peer: TPeerId);
  var i, j, p: Int32;
begin
  WriteLn('Peer removed: ', Peer.Name);
  if Assigned(_OnPeerLeft) then _OnPeerLeft(Peer);
  for i := High(_SendMessageQueue) downto 0 do
  begin
    with _SendMessageQueue[i] do
    for j := High(Chunks) downto 0 do
    begin
      with Chunks[j] do
      for p := High(Receivers) downto 0 do
      if (Receivers[p].Addr = Peer.Addr) and (Receivers[p].Port = Peer.Port) then
      begin
        specialize UArrDelete<TMessageReceiver>(Receivers, p);
      end;
      if Length(Chunks[j].Receivers) = 0 then
      begin
        specialize UArrDelete<TSendMessageChunk>(Chunks, j);
      end;
    end;
    if Length(_SendMessageQueue[i].Chunks) = 0 then
    begin
      specialize UArrDelete<TSendMessage>(_SendMessageQueue, i);
    end;
  end;
end;

procedure TChat.Update;
  var i: Int32;
  var t: UInt64;
begin
  RemoveOldPeers;
  NotifyPeers;
  t := GetTickCount64;
  for i := 0 to High(_SendMessageQueue) do
  if t - _SendMessageQueue[i].Timestamp > 2000 then
  begin
    ProcessSendMessage(_SendMessageQueue[i]);
  end;
  _MessagesLock.Enter;
  try
    if Assigned(_OnMessage) then
    for i := 0 to High(_Messages) do
    begin
      _OnMessage(_Messages[i].Peer, _Messages[i].Message);
    end;
    _Messages := nil;
  finally
    _MessagesLock.Leave;
  end;
end;

procedure TChat.Send(const Message: String);
  type PPacketMessage = ^TPacketMessage;
  var Msg: PPacketMessage;
  var ChunkCount, ChunkSize, ChunkRem: Int32;
  var SendMessage: TMessageSend;
  var CurId: UInt16;
  var p, i, n, m: Int32;
begin
  Receivers := GetPeers;
  CurId := _MsgId;
  Inc(_MsgId);
  if _MsgId = $ffff then _MsgId := 1;
  ChunkSize := BufferSizeUDP - SizeOf(TPacketMessage);
  ChunkCount := Length(Message) div ChunkSize;
  ChunkRem := Length(Message) mod ChunkSize;
  if ChunkRem > 0 then Inc(ChunkCount);
  SendMessage.Id := CurId;
  SendMessage.Timestamp := 0;
  SetLength(SendMessage.Chunks, ChunkCount);
  ChunkRem := Length(Message);
  SendMessage.Id := CurId;
  SetLength(SendMessage.Chunks, ChunkCount);
  m := 1;
  with SendMessage do
  for i := 0 to ChunkCount - 1 do
  begin
    Chunks[i].Success := False;
    n := UMin(ChunkRem, ChunkSize);
    SetLength(Chunks[i].Data, SizeOf(TPacketMessage) + n);
    Msg := PPacketMessage(@Chunks[i].Data[0]);
    Msg^.Id := CurId;
    Msg^.Marker := Marker;
    Msg^.Desc := UInt8(pd_message);
    Msg^.Count := UInt16(ChunkCount);
    Msg^.Index := UInt16(i);
    Move(Message[m], Chunks[i].Packet[SizeOf(TPacketMessage)], n);
    m += n;
  end;
  _PeersLock.Enter;
  try
    for i := 0 to High(_Peers) do
    begin
      specialize UArrAppend<TMessageSend>(
        _Peers[i].QueueSend, SendMessage
      );
    end;
  finally
    _PeersLock.Leave;
  end;
  ProcessSendMessages;
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

function TChat.DebugMessageQueue: Int32;
begin
  Result := Length(_SendMessageQueue);
end;

class operator TChat.TPeerId.=(const a, b: TPeerId): Boolean;
begin
  Result := (a.Addr = b.Addr) and (a.Port = b.Port);
end;

function TChat.TPeerId.SockAddr: TUSockAddr;
begin
  Result := TUSockAddr.Default;
  Result.sin_addr := Addr;
  Result.sin_port := HtoNs(Port);
end;

function TChat.TPeerId.CmpAddr(const PeerAddr: TUInAddr; const PeerPort: UInt16): Boolean;
begin
  Result := (Addr = PeerAddr) and (Port = PeerPort);
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
    MultiPackets[Result].Id := Id;
    SetLength(MultiPackets[Result].Chunks, ChunkCount);
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
    if r < SizeOf(PacketBase) then Continue;
    if (AddrFrom.sin_addr = Chat._MyAddr) and (NtoHs(AddrFrom.sin_port) = Chat._MyPort) then Continue;
    if PacketBase.Marker <> Marker then Continue;
    //WriteLn('Receivd: ', UNetNetAddrToStr(AddrFrom.sin_addr), ':', NtoHs(AddrFrom.sin_port));
    case TPacketDesc(PacketBase.Desc) of
      pd_ping:
      begin
        Chat.Sock.SendTo(@PacketPong, SizeOf(PacketPong), 0, @AddrFrom, SizeOf(AddrFrom));
      end;
      pd_close:
      begin
        Chat.RemovePeer(AddrFrom.sin_addr, NtoHs(AddrFrom.sin_port));
      end;
      pd_query:
      begin
        if r - SizeOf(PacketQuery) <> PacketQuery.NameLength then Continue;
        Chat.Sock.SendTo(@Chat.DiscoverPacket[0], Length(Chat.DiscoverPacket), 0, @AddrFrom, SizeOf(AddrFrom));
        SetLength(PeerName, PacketQuery.NameLength);
        Move(Buffer[SizeOf(PacketQuery)], PeerName[1], PacketQuery.NameLength);
        Chat.AddPeer(PeerName, AddrFrom.sin_addr, NtoHs(AddrFrom.sin_port));
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
        Chat.PacketConfirmed(
          AddrFrom.sin_addr,
          NtoHs(AddrFrom.sin_port),
          PacketReceived.Id,
          PacketReceived.Index
        );
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
          Chat.ReceiveMessage(
            AddrFrom.sin_addr, NtoHs(AddrFrom.sin_port),
            AssembleMultiPacket(MultiPackets[i])
          );
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
      //WriteLn('Broadcast: ', UNetNetAddrToStr(Addr.sin_addr) + ':', p);
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
    if Assigned(OnUpdate) then Synchronize(OnUpdate);
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
  Chat.Ptr.OnPeerJoined := @OnPeerJoined;
  Chat.Ptr.OnPeerLeft := @OnPeerLeft;
  Chat.Ptr.OnMessge := @OnMessage;
  Timer := TTimer.Create(Self);
  Timer.OnTimer := @OnTimer;
  Timer.Enabled := True;
end;

procedure TForm1.OnTimer(Sender: TObject);
begin
  LabelDebugMsgQueue1.Caption := 'Message Queue: ' + IntToStr(Chat.Ptr.DebugMessageQueue);
end;

procedure TForm1.OnPeerJoined(const Peer: TChat.TPeerId);
begin
  Memo1.Append(Peer.Name + ' Joined');
end;

procedure TForm1.OnPeerLeft(const Peer: TChat.TPeerId);
begin
  Memo1.Append(Peer.Name + ' Left');
end;

procedure TForm1.OnMessage(const Peer: TChat.TPeerId; const Message: String);
begin
  Memo1.Append('[' + Peer.Name + ']: ' + Message);
end;

procedure TForm1.SendMessage(const Msg: String);
begin
  Chat.Ptr.Send(Msg);
  Memo1.Append('[' + Chat.Ptr.Name + ']: ' + Msg);
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
  SendMessage(Edit1.Text);
  Edit1.Text := '';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Length(Edit1.Text) = 0 then Exit;
  SendMessage(Edit1.Text);
  Edit1.Text := '';
end;

procedure TForm1.Button2Click(Sender: TObject);
  var Buffer: String;
  var i: Int32;
begin
  SetLength(Buffer, 2000);
  for i := 1 to Length(Buffer) do Buffer[i] := 'A';
  SendMessage(Buffer);
end;

end.

