unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CommonUtils, NetUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    ListBox1: TListBox;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    var Beacon: TUNet.TBeaconRef;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormActivate(Sender: TObject);
begin
  if Timer1.Enabled then Exit;
  Beacon := TUNet.TBeacon.Create;
  Beacon.Ptr.Message := UNetMacAddrToStr(UNetLocalMacAddr);
  Timer1.Enabled := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Beacon.Ptr.Enabled then
  begin
    Beacon.Ptr.Enabled := False;
    ListBox1.Clear;
    Button1.Caption := 'Start';
  end
  else
  begin
    Beacon.Ptr.Active := CheckBox1.Checked;
    Beacon.Ptr.Enabled := True;
    Button1.Caption := 'Stop';
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
  var Peers: TUNet.TBeacon.TPeerArray;
  var i: Int32;
begin
  Peers := Beacon.Ptr.Peers;
  ListBox1.Items.BeginUpdate;
  ListBox1.Items.Clear;
  for i := 0 to High(Peers) do
  begin
    ListBox1.Items.Append(
      UNetNetAddrToStr(Peers[i].Addr) +
      ' (' + Peers[i].Name + ': ' + Peers[i].Message + ')'
    );
  end;
  ListBox1.Items.EndUpdate;
end;

end.

