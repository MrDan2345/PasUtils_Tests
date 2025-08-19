unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CommonUtils, NetUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ButtonStartStop1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Timer: TTimer;
    procedure ButtonStartStop1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    type TPeer = TUNet.TBeacon.TPeer;
    type TPeers = TUNet.TBeacon.TPeerArray;
    var Beacon: TUNet.TBeaconRef;
    var Peers: TPeers;
    var Sock: TUSocket;
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
  var i: Int32;
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
end;

procedure TForm1.OnStart;
begin

  Beacon.Ptr.Enabled := True;
end;

procedure TForm1.OnStop;
  var i: Int32;
begin
  Beacon.Ptr.Enabled := False;

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

end.

