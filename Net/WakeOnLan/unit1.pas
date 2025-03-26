unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  NetUtils, CommonUtils;

type TPeer = class
private
  var _Addr: TUInAddr;
  var _TimeStamp: UInt64;
  var _MacAddr: TUMacAddr;
  var _Name: String;
  var _AutoWake: Boolean;
  var _UIPanel: TPanel;
  var _UILabelName: TLabel;
  var _UILabelStaus: TLabel;
  var _UIButtonWake: TButton;
  var _UIButtonDelete: TButton;
  var _IdleUpdate: UInt64;
public
  property Addr: TUInAddr read _Addr;
  property MacAddr: TUMacAddr read _MacAddr;
  property Name: String read _Name;
  property AutoWake: Boolean read _AutoWake;
  constructor Create(const AAddr: TUInAddr);
  constructor Create(const Json: TUJson);
  destructor Destroy; override;
  procedure SetupUI(const ParentPanel: TPanel);
  procedure Update(const NewTimeStamp: UInt64; const Message: String);
  procedure UpdateUI;
  procedure IdleUpdate;
  procedure LoadJson(const Json: TUJson);
  procedure OnWake(Caller: TObject);
  procedure OnDelete(Caller: TObject);
end;
type TPeerArray = array of TPeer;

type TForm1 = class(TForm)
  LabelName1: TLabel;
  LabelMac1: TLabel;
  LabelAddress1: TLabel;
  PanelList1: TPanel;
  ScrollBox1: TScrollBox;
  Timer1: TTimer;
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
private
  var LocalName: String;
  var LocalAddr: TUInAddr;
  var LocalMac: TUMacAddr;
  var Beacon: TUNet.TBeaconRef;
  var Peers: TPeerArray;
  var PeersToDelete: TPeerArray;
public
  function FindPeer(const Addr: TUInAddr): TPeer;
  function FindOrAddPeer(const PeerInfo: TUNet.TBeacon.TPeer): TPeer;
  procedure OnTimer(Sender: TObject);
  procedure AddDummyPeer(const PeerName: String = 'Name'; const AddrOffset: Int8 = 0);
  procedure DeletePeer(const Peer: TPeer);
  procedure LoadPeers;
  procedure SavePeers;
end;

var Form1: TForm1;

implementation

{$R *.lfm}

constructor TPeer.Create(const AAddr: TUInAddr);
begin
  _Addr := AAddr;
  _AutoWake := False;
  _TimeStamp := 0;
  _UIPanel := nil;
  _IdleUpdate := 0;
end;

constructor TPeer.Create(const Json: TUJson);
begin
  _AutoWake := False;
  LoadJson(Json);
  _TimeStamp := 0;
  _UIPanel := nil;
  _IdleUpdate := 0;
end;

destructor TPeer.Destroy;
begin
  FreeAndNil(_UIPanel);
  inherited Destroy;
end;

procedure TPeer.SetupUI(const ParentPanel: TPanel);
begin
  _UIPanel := TPanel.Create(ParentPanel);
  _UIPanel.Parent := ParentPanel;
  with _UIPanel do
  begin
    Height := 32;
    Color := $505050;
    BevelOuter := bvSpace;
    BevelColor := $808080;
    BevelWidth := 2;
    BorderStyle := bsNone;
    Align := alTop;
    BorderSpacing.Around := 4;
  end;
  _UILabelName := TLabel.Create(_UIPanel);
  _UILabelName.Parent := _UIPanel;
  with _UILabelName do
  begin
    Font.Color := $ffffff;
    Align := alLeft;
    Layout := tlCenter;
    BorderSpacing.Left := 8;
    Caption := '?';
  end;
  _UIButtonDelete := TButton.Create(_UIPanel);
  _UIButtonDelete.Parent := _UIPanel;
  with _UIButtonDelete do
  begin
    Align := alRight;
    BorderSpacing.Left := 2;
    BorderSpacing.Right := 2;
    BorderSpacing.Top := 1;
    BorderSpacing.Bottom := 1;
    Caption := 'Delete';
    OnClick := @OnDelete;
  end;
  _UIButtonWake := TButton.Create(_UIPanel);
  _UIButtonWake.Parent := _UIPanel;
  with _UIButtonWake do
  begin
    Height := _UIButtonDelete.Height;
    AnchorToCompanion(akRight, 8, _UIButtonDelete);
    BorderSpacing.Left := 2;
    BorderSpacing.Right := 2;
    BorderSpacing.Top := 1;
    BorderSpacing.Bottom := 1;
    Caption := 'Wake';
    OnClick := @OnWake;
  end;
  _UILabelStaus := TLabel.Create(_UIPanel);
  _UILabelStaus.Parent := _UIPanel;
  with _UILabelStaus do
  begin
    Height := _UIButtonDelete.Height;
    AnchorToCompanion(akRight, 8, _UIButtonWake);
    Font.Color := $ffffff;
    Layout := tlCenter;
    BorderSpacing.Right := 8;
    Caption := '[?]';
  end;
end;

procedure TPeer.Update(const NewTimeStamp: UInt64; const Message: String);
  var Mac: TUMacAddr;
  var MsgArr: TUStrArray;
begin
  if _TimeStamp = NewTimeStamp then Exit;
  _TimeStamp := NewTimeStamp;
  MsgArr := UStrExplode(Message, '|');
  if Length(MsgArr) < 2 then Exit;
  _Name := MsgArr[0];
  Mac := UNetStrToMacAddr(MsgArr[1]);
  if (Length(_Name) = 0) or (not Mac.IsValid) then Exit;
  _MacAddr := Mac;
  UpdateUI;
end;

procedure TPeer.UpdateUI;
  var Cap: String;
begin
  _IdleUpdate := GetTickCount64;
  Cap := _Name + ' ' + UNetNetAddrToStr(_Addr) + '  [' + UNetMacAddrToStr(_MacAddr) + ']';
  if _AutoWake then Cap += ' (A)';
  _UILabelName.Caption := Cap;
  if (_TimeStamp = 0) or (GetTickCount64 - _TimeStamp > 60 * 1000) then
  begin
    _UILabelStaus.Caption := '[Offline]';
    _UILabelStaus.Font.Color := $000060;
  end
  else
  begin
    _UILabelStaus.Caption := '[Online]';
    _UILabelStaus.Font.Color := $009000;
  end;
end;

procedure TPeer.IdleUpdate;
begin
  if GetTickCount64 - _IdleUpdate <= 10 * 1000 then Exit;
  if UNetPing(_Addr) then
  begin
    _TimeStamp := GetTickCount64;
  end
  else if _AutoWake then
  begin
    OnWake(Self);
  end;
  UpdateUI;
end;

procedure TPeer.LoadJson(const Json: TUJson);
begin
  _Name := Json['name'].Value;
  _Addr := UNetStrToNetAddr(Json['addr'].Value);
  _MacAddr := UNetStrToMacAddr(Json['mac'].Value);
  _AutoWake := Json['auto_wake'].ValueAsBool;
end;

procedure TPeer.OnWake(Caller: TObject);
begin
  UNetWakeOnLan(_MacAddr);
end;

procedure TPeer.OnDelete(Caller: TObject);
begin
  Form1.DeletePeer(Self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LocalName := UNetHostName;
  LocalAddr := UNetLocalAddr;
  LocalMac := UNetLocalMacAddr;
  LabelName1.Caption := LocalName;
  LabelAddress1.Caption := UNetNetAddrToStr(LocalAddr);
  LabelMac1.Caption := UNetMacAddrToStr(LocalMac);
  //Caption := 'Wake On Lan ' + UNetNetAddrToStr(LocalAddr) + ' ' + UNetMacAddrToStr(LocalMac);
  Beacon := TUNet.TBeacon.Create;
  Beacon.Ptr.Active := True;
  Beacon.Ptr.BroadcastInterval := 30 * 1000;
  Beacon.Ptr.Message := LocalName + '|' + UNetMacAddrToStr(UNetLocalMacAddr);
  Beacon.Ptr.Port := 57210 + 22;
  Beacon.Ptr.Enabled := True;
  Timer1 := TTimer.Create(Self);
  Timer1.Interval := 1000;
  Timer1.OnTimer := @OnTimer;
  Timer1.Enabled := True;
  LoadPeers;
  //AddDummyPeer('Name1', 0);
  //AddDummyPeer('Name2', 1);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SavePeers;
  specialize UArrClear<TPeer>(Peers);
end;

function TForm1.FindPeer(const Addr: TUInAddr): TPeer;
  var i: Int32;
begin
  for i := 0 to High(Peers) do
  if Peers[i].Addr.Addr32 = Addr.Addr32 then Exit(Peers[i]);
  Result := nil;
end;

function TForm1.FindOrAddPeer(const PeerInfo: TUNet.TBeacon.TPeer): TPeer;
begin
  Result := FindPeer(PeerInfo.Addr);
  if not Assigned(Result) then
  begin
    Result := TPeer.Create(PeerInfo.Addr);
    Result.SetupUI(PanelList1);
    specialize UArrAppend<TPeer>(Peers, Result);
  end;
  Result.Update(PeerInfo.TimeStamp, PeerInfo.Message);
end;

procedure TForm1.OnTimer(Sender: TObject);
  var BeaconPeers: TUNet.TBeacon.TPeerArray;
  var Peer: TPeer;
  var i, PrevCount: Int32;
begin
  PrevCount := Length(Peers);
  if Length(PeersToDelete) > 0 then
  begin
    for i := 0 to High(PeersToDelete) do
    specialize UArrRemove<TPeer>(Peers, PeersToDelete[i]);
    specialize UArrClear<TPeer>(PeersToDelete);
  end;
  BeaconPeers := Beacon.Ptr.Peers;
  for i := 0 to High(BeaconPeers) do
  begin
    Peer := FindOrAddPeer(BeaconPeers[i]);
    Peer.Update(BeaconPeers[i].TimeStamp, BeaconPeers[i].Message);
  end;
  for i := 0 to High(Peers) do Peers[i].IdleUpdate;
  if Length(Peers) = PrevCount then Exit;
  SavePeers;
end;

procedure TForm1.AddDummyPeer(const PeerName: String; const AddrOffset: Int8);
  var Info: TUNet.TBeacon.TPeer;
  var Peer: TPeer;
begin
  Info.Addr := TUInAddr.LocalhostN;
  Info.Addr.Addr8[3] += AddrOffset;
  Info.Message := PeerName + '|18:c0:4d:d8:55:ec';
  Peer := FindOrAddPeer(Info);
  Peer.Update(Info.TimeStamp, Info.Message);
end;

procedure TForm1.DeletePeer(const Peer: TPeer);
  var i: Int32;
begin
  for i := 0 to High(PeersToDelete) do
  if PeersToDelete[i] = Peer then Exit;
  specialize UArrAppend<TPeer>(PeersToDelete, Peer);
end;

procedure TForm1.LoadPeers;
  var Json: TUJsonRef;
  var i: Int32;
begin
  if not FileExists('peers.json') then Exit;
  Json := TUJson.LoadFromFile('peers.json');
  if not Json.IsValid then Exit;
  if not Json.Ptr['peers'].IsArray then Exit;
  SetLength(Peers, Json.Ptr['peers'].Count);
  for i := 0 to High(Peers) do
  begin
    Peers[i] := TPeer.Create(Json.Ptr['peers'][i]);
    Peers[i].SetupUI(PanelList1);
    Peers[i].UpdateUI;
  end;
end;

procedure TForm1.SavePeers;
  var Json: TUJsonRef;
  var i: Int32;
begin
  Json := TUJson.Make;
  with Json.Ptr.AddArray('peers') do
  for i := 0 to High(Peers) do
  with AddObject() do
  begin
    AddValue('name', Peers[i].Name);
    AddValue('addr', UNetNetAddrToStr(Peers[i].Addr));
    AddValue('mac', UNetMacAddrToStr(Peers[i].MacAddr));
    if Peers[i].AutoWake then
    begin
      AddValue('auto_wake', True);
    end;
  end;
  Json.Ptr.SaveToFile('peers.json');
end;

end.

