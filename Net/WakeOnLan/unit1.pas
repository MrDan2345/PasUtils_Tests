unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, NetUtils;

type TForm1 = class(TForm)
  Button1: TButton;
  Edit1: TEdit;
  LabelMac1: TLabel;
  LabelAddress1: TLabel;
  procedure Button1Click(Sender: TObject);
  procedure FormCreate(Sender: TObject);
private
  var LocalAddr: TUInAddr;
  var LocalMac: TUMacAddr;
public

end;

type TEthernetHeader = packed record
  DestAddr: TUMacAddr;
  SrcAddr: TUMacAddr;
  Protocol: UInt16;
end;

const ETH_P_ALL = $0003; // Protocol for capturing all packets

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
  var Sock: TUSocket;
  var Msg: packed record
    EthHeader: TEthernetHeader;
    Sync: array[0..5] of UInt8;
    MacAddr: array[0..16 * 6 - 1] of UInt8;
  end;
  var i, BytesSent: Int32;
begin
  //https://en.wikipedia.org/wiki/Wake-on-LAN
  Msg.EthHeader.SrcAddr := UNetLocalMacAddr;
  Msg.EthHeader.DestAddr := UNetStrToMacAddr('01:23:45:67:89:AB');
  Msg.EthHeader.Protocol := htons(ETH_P_ALL);
  for i := 0 to 5 do Msg.Sync[i] := $ff;
  for i := 0 to 15 do Move(Msg.EthHeader.DestAddr, Msg.MacAddr[i * 6], SizeOf(TUMacAddr));
  Sock := TUSocket.Create(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
  try
    BytesSent := Sock.SendTo(@Msg, SizeOf(MSg), 0, nil, 0);
  finally
    Sock.Close;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LocalAddr := UNetLocalAddr;
  LocalMac := UNetLocalMacAddr;
  LabelAddress1.Caption := 'Address: ' + UNetNetAddrToStr(LocalAddr);
  LabelMac1.Caption := 'Mac: ' + UNetMacAddrToStr(LocalMac);
  Edit1.Text := UNetMacAddrToStr(LocalMac);
  Caption := 'Wake On Lan ' + UNetNetAddrToStr(LocalAddr) + ' ' + UNetMacAddrToStr(LocalMac);
end;

end.

