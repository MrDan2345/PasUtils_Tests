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
  const MaxNameLength: UInt8 = 255;
  type TBucketData = packed record
    var Addr: TUInAddr;
    var Port: UInt16;
    var Size: UInt16;
    var Packet: array[0..BufferSizeUDP - 1] of UInt8;
  end;
  type TBuckets = specialize TULinkedList<TBucketData>;
  type TBucket = TBuckets.TItem;
  type TPacketDesc = (
    pd_ping,
    pd_pong,
    pd_close,
    pd_query,
    pd_discover,
    pd_received,
    pd_message,
    pd_cmpl_req,
    pd_cmpl_ack
  );
  type TPacketBase = packed object
    var Marker: array[0..3] of AnsiChar;
    var Desc: UInt8;
  end;
  type TPacketMessage = packed object (TPacketBase)
    var Id: UInt16;
  end;
  type TPacketMessageCfrm = packed object (TPacketMessage)
    var Success: Boolean;
  end;
  type TPacketChunkMsg = packed object (TPacketMessage)
    var Index: UInt16;
    var Count: UInt16;
  end;
  type TPacketChunkCfrm = packed object (TPacketMessage)
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
    procedure Push;
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
    function IsComplete: Boolean;
    procedure AddChunk(const Index: Int32; const Data: TUInt8Array);
    function Assemble: TUInt8Array;
  end;
  type TPeer = record
    var Id: TPeerId;
    var TimeStamp: UInt64;
    var Queues: record
      var Send: array of TMessageSend;
      var Recv: array of TMessageRecv;
    end;
    function FindSend(const MsgId: UInt16): Int32;
    function FindRecv(const MsgId: UInt16): Int32;
  end;
  type TPeerArray = array of TPeer;
  var _Name: String;
  var _Sock: TUSocket;
  var _Enabled: Boolean;
  var _PortRange: array[0..1] of UInt16;
  var _Queue: TBuckets;
  var _QueueLock: TUCriticalSection;
  var _Peers: TPeerArray;
  var _Listener: TListener;
  var _Query: TQuery;
  var _Update: TUpdate;
  var _MyAddr: TUInAddr;
  var _MyPort: UInt16;
  var _MsgId: UInt16;
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
  procedure AddBucket(
    const PeerAddr: TUInAddr;
    const PeerPort: UInt16;
    const Buffer: Pointer;
    const Size: UInt16
  );
  procedure PacketConfirmed(
    const PeerIndex: Int32;
    const MsgId: UInt16;
    const Index: UInt16
  );
  procedure ReceiveMessage(
    const PeerIndex: Int32;
    const MsgId: UInt16;
    const ChunkCount: UInt16;
    const ChunkIndex: UInt16;
    const Msg: TUInt8Array
  );
  procedure ConfirmMessage(
    const PeerIndex: Int32;
    const MsgId: UInt16
  );
  procedure MessageConfirmed(
    const PeerIndex: Int32;
    const MsgId: UInt16;
    const Success: Boolean
  );
  function FindPeerNL(const PeerAddr: TUInAddr; const PeerPort: UInt16): Int32;
  function AddPeer(
    const PeerName: String;
    const PeerAddr: TUInAddr;
    const PeerPort: UInt16
  ): Int32;
  procedure RemoveOldPeers;
  function GetPeers: TPeerIdArray;
  procedure ProcessBuckets;
  procedure ProcessSendMessages;
  procedure PeerAdded(const PeerId: TPeerId);
  procedure PeerRemoved(const PeerId: TPeerId);
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
  _MsgId := 1;
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
  _Queue.Clear;
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

procedure TChat.AddBucket(
  const PeerAddr: TUInAddr;
  const PeerPort: UInt16;
  const Buffer: Pointer;
  const Size: UInt16
);
  var Bucket: TBucket;
begin
  _QueueLock.Enter;
  try
    Bucket := _Queue.NewItem;
    with Bucket do
    begin
      Data.Addr := PeerAddr;
      Data.Port := PeerPort;
      Data.Size := Size;
      Move(Buffer^, Data.Packet, Size);
    end;
  finally
    _QueueLock.Leave;
  end;
  _Update.Push;
end;

procedure TChat.PacketConfirmed(
  const PeerIndex: Int32;
  const MsgId: UInt16;
  const Index: UInt16
);
  var i: Int32;
begin
  with _Peers[PeerIndex] do
  for i := 0 to High(Queues.Send) do
  begin
    if Queues.Send[i].Id <> MsgId then Continue;
    if Index > High(Queues.Send[i].Chunks) then Exit;
    Queues.Send[i].Chunks[Index].Success := True;
  end;
end;

procedure TChat.ReceiveMessage(
  const PeerIndex: Int32;
  const MsgId: UInt16;
  const ChunkCount: UInt16;
  const ChunkIndex: UInt16;
  const Msg: TUInt8Array
);
  function FindOrCreateMultiPacket: Int32;
    function FindMultiPacket: Int32;
      var i: Int32;
    begin
      with _Peers[PeerIndex] do
      for i := 0 to High(Queues.Recv) do
      if Queues.Recv[i].Id = MsgId then
      begin
        Exit(i);
      end;
      Result := -1;
    end;
  begin
    Result := FindMultiPacket;
    if Result > -1 then Exit;
    with _Peers[PeerIndex] do
    begin
      Result := Length(Queues.Recv);
      SetLength(Queues.Recv, Result + 1);
      Queues.Recv[Result].Id := MsgId;
      SetLength(Queues.Recv[Result].Chunks, ChunkCount);
    end;
  end;
  var i: Int32;
  var MsgFull: TUInt8Array;
  var MsgStr: String;
  var PacketRcv: TPacketChunkCfrm;
  var SockAddr: TUSockAddr;
begin
  i := FindOrCreateMultiPacket;
  with _Peers[PeerIndex] do
  begin
    Queues.Recv[i].AddChunk(ChunkIndex, Msg);
    if Queues.Recv[i].IsComplete then
    begin
      if Assigned(_OnMessage) then
      begin
        MsgFull := Queues.Recv[i].Assemble;
        MsgStr := '';
        SetLength(MsgStr, Length(MsgFull));
        Move(Msg[0], MsgStr[1], Length(MsgFull));
        _OnMessage(Id, MsgStr);
      end;
    end;
  end;
  SockAddr := _Peers[PeerIndex].Id.SockAddr;
  PacketRcv.Marker := Marker;
  PacketRcv.Desc := UInt8(pd_received);
  PacketRcv.Id := MsgId;
  PacketRcv.Index := ChunkIndex;
  _Sock.SendTo(
    @PacketRcv, SizeOf(PacketRcv), 0,
    @SockAddr, SizeOf(SockAddr)
  );
end;

procedure TChat.ConfirmMessage(const PeerIndex: Int32; const MsgId: UInt16);
  var i: Int32;
  var PacketCfrm: TPacketMessageCfrm;
  var SockAddr: TUSockAddr;
begin
  PacketCfrm.Marker := Marker;
  PacketCfrm.Desc := UInt8(pd_cmpl_ack);
  PacketCfrm.Id := MsgId;
  PacketCfrm.Success := True;
  i := _Peers[PeerIndex].FindRecv(MsgId);
  if i > -1 then
  with _Peers[PeerIndex] do
  begin
    PacketCfrm.Success := Queues.Recv[i].IsComplete;
    if PacketCfrm.Success then
    begin
      specialize UArrDelete<TMessageRecv>(Queues.Recv, i);
    end;
  end;
  SockAddr := _Peers[PeerIndex].Id.SockAddr;
  _Sock.SendTo(
    @PacketCfrm, SizeOf(PacketCfrm), 0,
    @SockAddr, SizeOf(SockAddr)
  );
end;

procedure TChat.MessageConfirmed(
  const PeerIndex: Int32;
  const MsgId: UInt16;
  const Success: Boolean
);
  var i, j: Int32;
begin
  i := _Peers[PeerIndex].FindSend(MsgId);
  if i < 0 then Exit;
  if Success then
  begin
    specialize UArrDelete<TMessageSend>(
      _Peers[PeerIndex].Queues.Send, i
    );
  end
  else
  begin
    _Peers[PeerIndex].Queues.Send[i].TimeStamp := 0;
    with _Peers[PeerIndex].Queues.Send[i] do
    for j := 0 to High(Chunks) do
    begin
      Chunks[j].Success := False;
    end;
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
  var NewPeer: Boolean;
begin
  NewPeer := False;
  Result := FindPeerNL(PeerAddr, PeerPort);
  if Result = -1 then
  begin
    Result := Length(_Peers);
    SetLength(_Peers, Result + 1);
    _Peers[Result].Id.Addr := PeerAddr;
    _Peers[Result].Id.Port := PeerPort;
    NewPeer := True;
  end;
  _Peers[Result].Id.Name := PeerName;
  _Peers[Result].TimeStamp := GetTickCount64;
  if NewPeer then
  begin
    PeerAdded(_Peers[Result].Id);
  end;
end;

procedure TChat.RemoveOldPeers;
  var i: Int32;
  var t: UInt64;
begin
  t := GetTickCount64;
  for i := High(_Peers) downto 0 do
  if t - _Peers[i].TimeStamp > 15000 then
  begin
    PeerRemoved(_Peers[i].Id);
    specialize UArrDelete<TPeer>(_Peers, i);
  end;
end;

function TChat.GetPeers: TPeerIdArray;
  var i: Int32;
begin
  Result := nil;
  SetLength(Result, Length(_Peers));
  for i := 0 to High(_Peers) do
  begin
    Result[i] := _Peers[i].Id;
  end;
end;

procedure TChat.ProcessBuckets;
  procedure ProcessBucket(const Bucket: TBucketData);
    var PacketBase: TPacketBase absolute Bucket.Packet;
    var PacketMessage: TPacketChunkMsg absolute Bucket.Packet;
    var PacketReceived: TPacketChunkCfrm absolute Bucket.Packet;
    var PacketQuery: TPacketQuery absolute Bucket.Packet;
    var PacketCfrm: TPacketMessageCfrm absolute Bucket.Packet;
    var PkCount, PkIndex: UInt16;
    var Msg: TUInt8Array;
    var PeerName: String;
    var PeerIndex: Int32;
    var SockAddr: TUSockAddr;
    var i: Int32;
  begin
    PeerIndex := FindPeerNL(Bucket.Addr, Bucket.Port);
    SockAddr := TUSockAddr.Default;
    SockAddr.sin_addr := Bucket.Addr;
    SockAddr.sin_port := HtoNs(Bucket.Port);
    case TPacketDesc(PacketBase.Desc) of
      pd_ping:
      begin
        //_Sock.SendTo(@PacketPong, SizeOf(PacketPong), 0, @AddrFrom, SizeOf(AddrFrom));
      end;
      pd_close:
      begin
        if PeerIndex = -1 then Exit;
        specialize UArrDelete<TPeer>(_Peers, PeerIndex);
      end;
      pd_query:
      begin
        if Bucket.Size < SizeOf(PacketQuery) then Exit;
        if PacketQuery.NameLength > MaxNameLength then Exit;
        if Bucket.Size - SizeOf(PacketQuery) <> PacketQuery.NameLength then Exit;
        PeerName := '';
        SetLength(PeerName, PacketQuery.NameLength);
        Move(Bucket.Packet[SizeOf(PacketQuery)], PeerName[1], PacketQuery.NameLength);
        AddPeer(PeerName, Bucket.Addr, Bucket.Port);
        _Sock.SendTo(
          @DiscoverPacket[0], Length(DiscoverPacket), 0,
          @SockAddr, SizeOf(SockAddr)
        );
      end;
      pd_discover:
      begin
        if Bucket.Size < SizeOf(PacketQuery) then Exit;
        if PacketQuery.NameLength > MaxNameLength then Exit;
        if Bucket.Size - SizeOf(PacketQuery) <> PacketQuery.NameLength then Exit;
        SetLength(PeerName, PacketQuery.NameLength);
        Move(Bucket.Packet[SizeOf(PacketQuery)], PeerName[1], PacketQuery.NameLength);
        AddPeer(PeerName, Bucket.Addr, Bucket.Port);
      end;
      pd_received:
      begin
        if PeerIndex = -1 then Exit;
        PacketConfirmed(
          PeerIndex,
          PacketReceived.Id,
          PacketReceived.Index
        );
      end;
      pd_message:
      begin
        if PeerIndex = -1 then Exit;
        PkCount := PacketMessage.Count;
        PkIndex := PacketMessage.Index;
        Msg := nil;
        SetLength(Msg, Bucket.Size - SizeOf(PacketMessage));
        Move(Bucket.Packet[SizeOf(PacketMessage)], Msg[0], Length(Msg));
        ReceiveMessage(PeerIndex, PacketMessage.Id, PkCount, PkIndex, Msg);
      end;
      pd_cmpl_req:
      begin
        if PeerIndex = -1 then Exit;
        ConfirmMessage(PeerIndex, PacketMessage.Id);
      end;
      pd_cmpl_ack:
      begin
        if PeerIndex = -1 then Exit;
        MessageConfirmed(PeerIndex, PacketCfrm.Id, PacketCfrm.Success);
      end;
    end;
  end;
  var Bucket: TBucket;
begin
  _QueueLock.Enter;
  try
    Bucket := _Queue.List;
    while Assigned(Bucket) do
    begin
      ProcessBucket(Bucket.Data);
      Bucket := Bucket.Next;
    end;
    _Queue.Clear;
  finally
    _QueueLock.Leave;
  end;
end;

procedure TChat.ProcessSendMessages;
  var SockAddr: TUSockAddr;
  var p, m, c: Int32;
  var t: UInt64;
  var AllSent: Boolean;
  var PacketMessageCfrm: TPacketMessage;
begin
  t := GetTickCount64;
  for p := 0 to High(_Peers) do
  begin
    SockAddr := _Peers[p].Id.SockAddr;
    with _Peers[p] do
    for m := 0 to High(Queues.Send) do
    with Queues.Send[m] do
    if t - TimeStamp > 2000 then
    begin
      TimeStamp := t;
      AllSent := True;
      for c := 0 to High(Chunks) do
      if not Chunks[c].Success then
      begin
        AllSent := False;
        _Sock.SendTo(
          @Chunks[c].Data[0], Length(Chunks[c].Data),
          0, @SockAddr, SizeOf(SockAddr)
        );
      end;
      if AllSent then
      begin
        PacketMessageCfrm.Marker := Marker;
        PacketMessageCfrm.Desc := UInt8(pd_cmpl_req);
        PacketMessageCfrm.Id := Id;
        _Sock.SendTo(
          @PacketMessageCfrm, SizeOf(PacketMessageCfrm),
          0, @SockAddr, SizeOf(SockAddr)
        );
      end;
    end;
  end;
end;

procedure TChat.PeerAdded(const PeerId: TPeerId);
begin
  WriteLn('Peer added: ', PeerId.Name);
  if Assigned(_OnPeerJoined) then _OnPeerJoined(PeerId);
end;

procedure TChat.PeerRemoved(const PeerId: TPeerId);
  var i, j, p: Int32;
begin
  WriteLn('Peer removed: ', PeerId.Name);
  if Assigned(_OnPeerLeft) then _OnPeerLeft(PeerId);
end;

procedure TChat.Update;
begin
  ProcessBuckets;
  ProcessSendMessages;
  RemoveOldPeers;
end;

procedure TChat.Send(const Message: String);
  type PPacketMessage = ^TPacketChunkMsg;
  var Msg: PPacketMessage;
  var ChunkCount, ChunkSize, ChunkRem: Int32;
  var SendMessage: TMessageSend;
  var CurId: UInt16;
  var i, n, m: Int32;
begin
  CurId := _MsgId;
  Inc(_MsgId);
  if _MsgId = $ffff then _MsgId := 1;
  ChunkSize := BufferSizeUDP - SizeOf(TPacketChunkMsg);
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
    SetLength(Chunks[i].Data, SizeOf(TPacketChunkMsg) + n);
    Msg := PPacketMessage(@Chunks[i].Data[0]);
    Msg^.Id := CurId;
    Msg^.Marker := Marker;
    Msg^.Desc := UInt8(pd_message);
    Msg^.Count := UInt16(ChunkCount);
    Msg^.Index := UInt16(i);
    Move(Message[m], Chunks[i].Data[SizeOf(TPacketChunkMsg)], n);
    m += n;
  end;
  for i := 0 to High(_Peers) do
  begin
    specialize UArrAppend<TMessageSend>(
      _Peers[i].Queues.Send, SendMessage
    );
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
  _Queue.Cached := True;
end;

destructor TChat.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

function TChat.DebugMessageQueue: Int32;
begin
  Result := 0;//Length(_SendMessageQueue);
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
  var PacketBase: TPacketBase absolute Buffer;
  var MarkerCheck: array[0..3] of AnsiChar;
  var AddrFrom: TUSockAddr;
  var SockLen: TUSockLen;
  var i, r: Int32;
begin
  UClear(MarkerCheck, SizeOf(MarkerCheck));
  while not Terminated do
  begin
    SockLen := SizeOf(AddrFrom);
    r := Chat.Sock.RecvFrom(@Buffer, BufferSizeUDP, 0, @AddrFrom, @SockLen);
    if r < SizeOf(PacketBase) then Continue;
    if (AddrFrom.sin_addr = Chat._MyAddr) and (NtoHs(AddrFrom.sin_port) = Chat._MyPort) then Continue;
    if PacketBase.Marker <> Marker then Continue;
    Chat.AddBucket(AddrFrom.sin_addr, NtoHs(AddrFrom.sin_port), @Buffer, UInt16(r));
    //Continue;
    //WriteLn('Receivd: ', UNetNetAddrToStr(AddrFrom.sin_addr), ':', NtoHs(AddrFrom.sin_port));
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
    Event.Unsignal;
  end;
end;

procedure TChat.TUpdate.TerminatedSet;
begin
  inherited TerminatedSet;
  Event.Signal;
end;

procedure TChat.TUpdate.Push;
begin
  Event.Signal;
end;

function TChat.TMessageRecv.IsComplete: Boolean;
  var i: Int32;
begin
  for i := 0 to High(Chunks) do
  if not Chunks[i].Success then
  begin
    Exit(False);
  end;
  Result := True;
end;

procedure TChat.TMessageRecv.AddChunk(
  const Index: Int32;
  const Data: TUInt8Array
);
begin
  if (Index < 0) or (Index > High(Chunks)) then Exit;
  if Length(Chunks[Index].Data) > 0 then Exit;
  Chunks[Index].Data := Data;
  Chunks[Index].Success := True;
end;

function TChat.TMessageRecv.Assemble: TUInt8Array;
  var i, n: Int32;
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

function TChat.TPeer.FindSend(const MsgId: UInt16): Int32;
  var i: Int32;
begin
  for i := 0 to High(Queues.Send) do
  if Queues.Send[i].Id = MsgId then
  begin
    Exit(i);
  end;
  Result := -1;
end;

function TChat.TPeer.FindRecv(const MsgId: UInt16): Int32;
  var i: Int32;
begin
  for i := 0 to High(Queues.Recv) do
  if Queues.Recv[i].Id = MsgId then
  begin
    Exit(i);
  end;
  Result := -1;
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

