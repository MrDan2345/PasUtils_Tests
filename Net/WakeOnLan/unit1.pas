unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, NetUtils, Sockets;

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
    //EthHeader: TEthernetHeader;
    Sync: array[0..5] of UInt8;
    MacAddr: array[0..16 * 6 - 1] of UInt8;
  end;
  var Addr: TUSockAddr;
  var Mac: TUMacAddr;
  var i, BytesSent: Int32;
begin
  //https://en.wikipedia.org/wiki/Wake-on-LAN
  //Msg.EthHeader.SrcAddr := UNetLocalMacAddr;
  //Msg.EthHeader.DestAddr := UNetStrToMacAddr('18:c0:4d:d8:55:ec');
  //Msg.EthHeader.Protocol := htons(ETH_P_ALL);
  Mac := UNetStrToMacAddr('18:c0:4d:d8:55:ec');
  for i := 0 to 5 do Msg.Sync[i] := $ff;
  for i := 0 to 15 do Move(Mac, Msg.MacAddr[i * 6], SizeOf(TUMacAddr));
  //Sock := TUSocket.Create(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
  Sock := TUSocket.CreateUDP();
  Sock.SetSockOpt(SO_BROADCAST, 1);
  try
    Addr := TUSockAddr.Default;
    Addr.sin_addr := TUInAddr.Broadcast;
    Addr.sin_port := 7;
    BytesSent := Sock.SendTo(@Msg, SizeOf(MSg), 0, @Addr, SizeOf(Addr));
    Addr.sin_addr := UNetStrToNetAddr('192.168.1.129');
    Addr.sin_port := 7;
    BytesSent := Sock.SendTo(@Msg, SizeOf(MSg), 0, @Addr, SizeOf(Addr));
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

