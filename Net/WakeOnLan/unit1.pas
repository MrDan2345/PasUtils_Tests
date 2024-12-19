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
  var _UIPanel: TPanel;
public
  property Addr: TUInAddr read _Addr;
  constructor Create(const AAddr: TUInAddr);
  destructor Destroy; override;
  procedure SetupUI(const ParentPanel: TPanel);
  procedure Update(const NewTimeStamp: UInt64; const Message: String);
end;
type TPeerArray = array of TPeer;

type TForm1 = class(TForm)
  LabelName1: TLabel;
  LabelMac1: TLabel;
  LabelAddress1: TLabel;
  PanelList1: TPanel;
  Timer1: TTimer;
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
private
  var LocalName: String;
  var LocalAddr: TUInAddr;
  var LocalMac: TUMacAddr;
  var Beacon: TUNet.TBeaconRef;
  var Peers: TPeerArray;
public
  function FindPeer(const Addr: TUInAddr): TPeer;
  function FindOrAddPeer(const PeerInfo: TUNet.TBeacon.TPeer): TPeer;
  procedure OnTimer(Sender: TObject);
end;

var Form1: TForm1;

implementation

{$R *.lfm}

constructor TPeer.Create(const AAddr: TUInAddr);
begin
  _Addr := AAddr;
  _TimeStamp := 0;
  _UIPanel := nil;
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
    Color := $808080;
    BevelOuter := bvSpace;
    BevelColor := $c0c0c0;
    BevelWidth := 2;
    BorderStyle := bsNone;
    Align := alTop;
    BorderSpacing.Around := 4;
  end;
end;

procedure TPeer.Update(const NewTimeStamp: UInt64; const Message: String);
begin
  if _TimeStamp = NewTimeStamp then Exit;
  _TimeStamp := NewTimeStamp;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LocalName := UNetHostName;
  LocalAddr := UNetLocalAddr;
  LocalMac := UNetLocalMacAddr;
  LabelName1.Caption := LocalName;
  LabelAddress1.Caption := UNetNetAddrToStr(LocalAddr);
  LabelMac1.Caption := UNetMacAddrToStr(LocalMac);
  Caption := 'Wake On Lan ' + UNetNetAddrToStr(LocalAddr) + ' ' + UNetMacAddrToStr(LocalMac);
  Beacon := TUNet.TBeacon.Create;
  Beacon.Ptr.Active := True;
  Beacon.Ptr.BroadcastInterval := 30 * 1000;
  Beacon.Ptr.Message := UNetMacAddrToStr(UNetLocalMacAddr);
  Beacon.Ptr.Port := 57210 + 22;
  Beacon.Ptr.Enabled := True;
  Timer1 := TTimer.Create(Self);
  Timer1.Interval := 1000;
  Timer1.OnTimer := @OnTimer;
  Timer1.Enabled := True;
  //AddPeer;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  specialize UArrClear<TPeer>(Peers);
end;

function TForm1.FindPeer(const Addr: TUInAddr): TPeer;
  var i: Int32;
begin
  for i := 0 to High(Peers) do
  if Peers[i].Addr = Addr then Exit(Peers[i]);
  Result := nil;
end;

function TForm1.FindOrAddPeer(const PeerInfo: TUNet.TBeacon.TPeer): TPeer;
  var Peer: TPeer;
begin
  Peer := FindPeer(PeerInfo.Addr);
  if not Assigned(Peer) then
  begin
    Peer := TPeer.Create(PeerInfo.Addr);
    Peer.SetupUI(PanelList1);
    specialize UArrAppend<TPeer>(Peers, Peer);
  end;
  Peer.Update(PeerInfo.TimeStamp, PeerInfo.Message);
end;

procedure TForm1.OnTimer(Sender: TObject);
  var BeaconPeers: TUNet.TBeacon.TPeerArray;
  var Peer: TPeer;
  var i: Int32;
begin
  BeaconPeers := Beacon.Ptr.Peers;
  for i := 0 to High(BeaconPeers) do
  begin
    Peer := FindOrAddPeer(BeaconPeers[i]);
    Peer.Update(BeaconPeers[i].TimeStamp, BeaconPeers[i].Message);
  end;
end;

end.

