unit Unit1;

{$include PasUtilsMode.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CommonUtils, NetUtils, LCLType;

type TChat = class (TURefClass)
private
  const BufferSizeUDP = 1408;
  type TPacketDesc = (pd_ping, pd_pong, pd_query, pd_discover, pd_received, pd_message);
  type TPacketBase = packed object
    var Marker: array[0..3] of AnsiChar;
    var Desc: UInt8;
  end;
  type TPacketMessage = packed object (TPacketBase)
    var Id: UInt16;
    function IsMulti: Boolean; inline;
  end;
  type TPacketMessageMulti = packed object (TPacketMessage)
    var Count: UInt16;
    var Index: UInt16;
  end;
  type TPacketReceived = packed object (TPacketMessage)
    var Index: UInt16;
  end;
  type TPacketQuery = packed object (TPacketMessage)
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
  type TPeer = record
    Name: String;
    Addr: TUInAddr;
    Port: UInt16;
  end;
  var _Name: String;
  var _Sock: TUSocket;
  var _Enabled: Boolean;
  var _PortRange: array[0..1] of UInt16;
  var _Peers: array of TPeer;
  var _PeersLock: TUCriticalSection;
  var _Listener: TListener;
  var _Query: TQuery;
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
public
  property Name: String read _Name write _Name;
  property Sock: TUSocket read _Sock;
  property Enabled: Boolean read _Enabled write SetEnabled;
  property PortRange[const Index: Int8]: UInt16 read GetPortRange write SetPortRange;
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
  Form1.Memo1.Append('Listening: ' + IntToStr(NToHs(Addr.sin_port)));
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
  _Listener.Start;
  _Query.Start;
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
    Form1.Memo1.Append(PeerName);
  finally
    _PeersLock.Leave;
  end;
end;

procedure TChat.Send(const Message: String);
begin

end;

constructor TChat.Create;
begin
  _Name := UNetHostName;
  _Sock := TUSocket.Invalid;
  _Enabled := False;
  _PortRange[0] := 61390;
  _PortRange[1] := 61390;
end;

destructor TChat.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

procedure TChat.TListener.Execute;
  var Buffer: array[0..BufferSizeUDP - 1] of UInt16;
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
  var PacketMessage: TPacketMessageMulti absolute Buffer;
  var PacketReceived: TPacketReceived absolute Buffer;
  var PacketQuery: TPacketQuery absolute Buffer;
  var PkCount, PkIndex: UInt16;
  var MultiPacket: Boolean;
  var Msg: TUInt8Array;
  var PacketRcv: TPacketReceived;
  var HeaderSize: Int32;
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
    Form1.Memo1.Append('Receivd: ' + UNetNetAddrToStr(AddrFrom.sin_addr) + ':' + IntToStr(NtoHs(AddrFrom.sin_port)));
    if r <= SizeOf(PacketBase) then Continue;
    if PacketBase.Marker <> Marker then Continue;
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
        if PacketReceived.Id and $8000 > 0 then
        begin
          PkIndex := PacketReceived.Index;
        end;
        Chat.PacketConfirmed(PacketReceived.Id, PkIndex);
      end;
      pd_message:
      begin
        MultiPacket := PacketMessage.IsMulti;
        if MultiPacket then
        begin
          PkCount := PacketMessage.Count;
          PkIndex := PacketMessage.Index;
          HeaderSize := SizeOf(TPacketMessageMulti);
        end
        else
        begin
          HeaderSize := SizeOf(TPacketMessage);
        end;
        SetLength(Msg, r - HeaderSize);
        Move(Buffer[HeaderSize], Msg[0], Length(Msg));
        PacketRcv.Id := PacketMessage.Id;
        if MultiPacket then
        begin
          i := FindOrCreateMultiPacket(PacketMessage.Id, PkCount);
          AddMultiChunk(MultiPackets[i], PkIndex, Msg);
          if MultiPacketReady(MultiPackets[i]) then
          begin
            Chat.ReceiveMessage(AddrFrom.sin_addr, AssembleMultiPacket(MultiPackets[i]));
            specialize UArrDelete<TMultiPacket>(MultiPackets, i);
          end;
          PacketRcv.Index := PkIndex;
          Chat.Sock.SendTo(
            @PacketRcv, SizeOf(TPacketReceived), 0, @AddrFrom, SizeOf(AddrFrom)
          );
        end
        else
        begin
          Chat.ReceiveMessage(AddrFrom.sin_addr, Msg);
          Chat.Sock.SendTo(
            @PacketRcv, SizeOf(TPacketMessage), 0, @AddrFrom, SizeOf(AddrFrom)
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
      Form1.Memo1.Append('Broadcast: ' + UNetNetAddrToStr(Addr.sin_addr) + ':' + IntToStr(p));
    end;
    Event.WaitFor(5000);
  end;
end;

procedure TChat.TQuery.TerminatedSet;
begin
  inherited TerminatedSet;
  Event.Signal;
end;

function TChat.TPacketMessage.IsMulti: Boolean;
begin
  Result := Id and $8000 > 0;
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

