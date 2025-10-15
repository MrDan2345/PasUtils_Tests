program evpkdf_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

procedure Run;
  var Key: TUInt8Array;
  var IV: TUInt8Array;
begin
  WriteLn('Testing EvpKDF SHA256');
  Key := UEvpKDF_SHA2_256(
    UStrToBytes('password'), UHexToBytes('0123456789abcdef'),
    SizeOf(TUAES.TKey256), SizeOf(TUAES.TInitVector), 1, IV
  );
  WriteLn('Key:          ', LowerCase(UBytesToHex(Key)));
  WriteLn('Expected Key: ', '9f2c9f8c1941ab1bf71f5bca59735d84ab62ee82d92cc21bb0daab35d6f8a2d8');
  WriteLn('IV:          ', LowerCase(UBytesToHex(IV)));
  WriteLn('Expected IV: ', '1165313d766e406bdea7bcae599a3b22');
  WriteLn();
  WriteLn('Testing EvpKDF MD5');
  Key := UEvpKDF_MD5(
    UStrToBytes('password'), UHexToBytes('0123456789abcdef'),
    SizeOf(TUAES.TKey256), SizeOf(TUAES.TInitVector), 1, IV
  );
  WriteLn('Key:          ', LowerCase(UBytesToHex(Key)));
  WriteLn('Expected Key: ', '45cd1c2d6cd6fa6db6d72683b58ee06c3a66b9f05eadaa053b675d62203b0608');
  WriteLn('IV:          ', LowerCase(UBytesToHex(IV)));
  WriteLn('Expected IV: ', 'ebb6c987d43b0764fb7d915e2b88e28d');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

