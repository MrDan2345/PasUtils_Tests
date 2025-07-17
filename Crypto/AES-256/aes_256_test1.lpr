program aes_256_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function BytesToKey(const Bytes: TUInt8Array): TUAES256Key;
  var n: Int32;
begin
  n := UMin(Length(Bytes), Length(TUAES256Key));
  Move(Bytes[0], Result[0], n);
end;

procedure RunAESTests;
  procedure RunTest(const Name, KeyStr, DataHex, Expected: String);
    var Key: TUAES256Key;
    var Data, Cipher: TUInt8Array;
    var CipherStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_PKCS7_ECB_256(Data, Key);
    CipherStr := UBytesToHex(Cipher);
    WriteLn('Key: ', KeyStr);
    WriteLn('Data: ', DataHex);
    if LowerCase(Expected) = LowerCase(CipherStr) then
    begin
      WriteLn('SUCCESS');
    end
    else
    begin
      WriteLn('FAIL');
    end;
    WriteLn('Actual:   ', LowerCase(CipherStr));
    WriteLn('Expected: ', LowerCase(Expected));
    WriteLn();
  end;
begin
  RunTest(
    '--- Testing AES-256 ECB: Single Block ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    '6bc1bee22e409f96e93d7e117393172a',
    'f3eed1bdb5d2a03c064b5a7e3db181f84c45dfb3b3b484ec35b0512dc8c1c4d6'
  );
  RunTest(
    '--- Testing AES-256 ECB: Multi-Block & Padding ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '591ccb10d410ed26dc5ba74a31362870b6ed21b99ca6f4f9f153e7b1beafed1d4c45dfb3b3b484ec35b0512dc8c1c4d6'
  );
  RunTest(
    '--- Testing AES-256 ECB: Empty Data ---',
    '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
    '',
    '76f92b99011802440efaf3539934b7b5'
  );
end;

procedure Run;
begin
  RunAESTests;
end;

begin
  Run;
end.

