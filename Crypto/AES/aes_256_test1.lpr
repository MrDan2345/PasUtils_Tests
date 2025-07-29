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

function BytesToKey(const Bytes: TUInt8Array): TUAES.TKey256;
  var n: Int32;
begin
  n := UMin(Length(Bytes), Length(TUAES.TKey256));
  Move(Bytes[0], Result[0], n);
end;

function HexToIV(const Hex: String): TUAES.TInitVector;
  var Bytes: TUInt8Array;
  var i, n: Int32;
begin
  Bytes := UHexToBytes(Hex);
  n := UMin(Length(Bytes), Length(Result));
  Move(Bytes[0], Result[0], n);
  for i := n to High(Result) do
  begin
    Result[i] := 0;
  end;
end;

function IVToHex(const IV: TUAES.TInitVector): String;
  var i: Int32;
begin
  Result := '';
  for i := 0 to High(IV) do
  begin
    Result += IntToHex(IV[i], 2);
  end;
end;

procedure RunAESTests;
  var IV: TUAES.TInitVector;
  procedure RunTestECB(const Name, KeyStr, DataHex, Expected: String);
    var Key: TUAES.TKey256;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_PKCS7_ECB_256(Data, Key);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_PKCS7_ECB_256(Cipher, Key);
    DecryptStr := UBytesToHex(Decrypt);
    WriteLn('Key: ', KeyStr);
    WriteLn('Data: ', DataHex);
    if LowerCase(Expected) = LowerCase(CipherStr) then
    begin
      WriteLn('Encrypt SUCCESS');
    end
    else
    begin
      WriteLn('Encrypt FAIL');
    end;
    WriteLn('Actual:   ', LowerCase(CipherStr));
    WriteLn('Expected: ', LowerCase(Expected));
    if LowerCase(DataHex) = LowerCase(DecryptStr) then
    begin
      WriteLn('Decrypt SUCCESS');
    end
    else
    begin
      WriteLn('Decrypt FAIL');
    end;
    WriteLn('Decrypted:   ', LowerCase(DecryptStr));
    WriteLn();
  end;
  procedure RunTestCBC(const Name, KeyStr, DataHex, Expected: String);
    var Key: TUAES.TKey256;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_PKCS7_CBC_256(Data, Key, IV);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_PKCS7_CBC_256(Cipher, Key, IV);
    DecryptStr := UBytesToHex(Decrypt);
    WriteLn('Key: ', KeyStr);
    WriteLn('Data: ', DataHex);
    if LowerCase(Expected) = LowerCase(CipherStr) then
    begin
      WriteLn('Encrypt SUCCESS');
    end
    else
    begin
      WriteLn('Encrypt FAIL');
    end;
    WriteLn('Actual:   ', LowerCase(CipherStr));
    WriteLn('Expected: ', LowerCase(Expected));
    if LowerCase(DataHex) = LowerCase(DecryptStr) then
    begin
      WriteLn('Decrypt SUCCESS');
    end
    else
    begin
      WriteLn('Decrypt FAIL');
    end;
    WriteLn('Decrypted:   ', LowerCase(DecryptStr));
    WriteLn();
  end;
  procedure RunTestCTR(const Name, KeyStr, DataHex, Expected: String);
    var Key: TUAES.TKey256;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_CTR_256(Data, Key, IV);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_CTR_256(Cipher, Key, IV);
    DecryptStr := UBytesToHex(Decrypt);
    WriteLn('Key: ', KeyStr);
    WriteLn('Data: ', DataHex);
    if LowerCase(Expected) = LowerCase(CipherStr) then
    begin
      WriteLn('Encrypt SUCCESS');
    end
    else
    begin
      WriteLn('Encrypt FAIL');
    end;
    WriteLn('Actual:   ', LowerCase(CipherStr));
    WriteLn('Expected: ', LowerCase(Expected));
    if LowerCase(DataHex) = LowerCase(DecryptStr) then
    begin
      WriteLn('Decrypt SUCCESS');
    end
    else
    begin
      WriteLn('Decrypt FAIL');
    end;
    WriteLn('Decrypted:   ', LowerCase(DecryptStr));
    WriteLn();
  end;
  procedure RunTestGCM(const Name, KeyStr, DataHex, Expected, Extra: String);
    var Key: TUAES.TKey256;
    var Data, Cipher, Decrypt, AAD: TUInt8Array;
    var CipherStr, DecryptStr: String;
    var AuthTag, DecryptTag: TUAES.TTag;
    var AuthPassed: Boolean;
    var i: Int32;
  begin
    WriteLn(Name);
    AAD := UStrToBytes(Extra);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_GCM_256(Data, Key, IV, AAD, AuthTag);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_GCM_256(Cipher, Key, IV, AAD, DecryptTag);
    DecryptStr := UBytesToHex(Decrypt);
    WriteLn('Key: ', KeyStr);
    WriteLn('Data: ', DataHex);
    if LowerCase(Expected) = LowerCase(CipherStr) then
    begin
      WriteLn('Encrypt SUCCESS');
    end
    else
    begin
      WriteLn('Encrypt FAIL');
    end;
    WriteLn('Actual:   ', LowerCase(CipherStr));
    WriteLn('Expected: ', LowerCase(Expected));
    if LowerCase(DataHex) = LowerCase(DecryptStr) then
    begin
      WriteLn('Decrypt SUCCESS');
    end
    else
    begin
      WriteLn('Decrypt FAIL');
    end;
    WriteLn('Decrypted:   ', LowerCase(DecryptStr));
    AuthPassed := True;
    for i := 0 to High(AuthTag) do
    if AuthTag[i] <> DecryptTag[i] then
    begin
      AuthPassed := False;
      Break;
    end;
    if AuthPassed then
    begin
      WriteLn('Authentication SUCCESS');
    end
    else
    begin
      WriteLn('Authentication FAIL');
    end;
    WriteLn('AuthTag:    ', LowerCase(UBytesToHex(AuthTag)));
    WriteLn('DecryptTag: ', LowerCase(UBytesToHex(DecryptTag)));
    WriteLn();
  end;
begin
  WriteLn('----- ECB -----');
  RunTestECB(
    '--- Testing AES-256 ECB: Single Block ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    '6bc1bee22e409f96e93d7e117393172a',
    'f3eed1bdb5d2a03c064b5a7e3db181f84c45dfb3b3b484ec35b0512dc8c1c4d6'
  );
  RunTestECB(
    '--- Testing AES-256 ECB: Multi-Block & Padding ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '591ccb10d410ed26dc5ba74a31362870b6ed21b99ca6f4f9f153e7b1beafed1d4c45dfb3b3b484ec35b0512dc8c1c4d6'
  );
  RunTestECB(
    '--- Testing AES-256 ECB: Empty Data ---',
    '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
    '',
    '76f92b99011802440efaf3539934b7b5'
  );
  WriteLn('----- CBC -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('IV: ', LowerCase(IVToHex(IV)));
  RunTestCBC(
    '--- Testing AES-256 CBC: Single Block ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    '6bc1bee22e409f96e93d7e117393172a',
    'edaa2506e0d27c14040adf8a9542faec06a2ca2b2e70e75e9f70fc2f49767845'
  );
  RunTestCBC(
    '--- Testing AES-256 CBC: Multi-Block & Padding ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '4b75a8cdf3af944a0ffc376e25b66293a41448cdc97395005d358833c24a28ced5d6dbccdc2af4f037a54c2a704f6b23'
  );
  RunTestCBC(
    '--- Testing AES-256 CBC: Empty Data ---',
    '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
    '',
    '3cfe74262057a0f2feaf451981449e6d'
  );
  WriteLn('----- CTR -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('Nonce: ', LowerCase(IVToHex(IV)));
  RunTestCTR(
    '--- Testing AES-256 CTR: Short Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    '6bc1bee22e409f96e93d7e117393172a',
    '010785a5fc4338ada3160a256a9a6a54'
  );
  RunTestCTR(
    '--- Testing AES-256 CTR: Longer Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    'c4ebb110cc000ba7d49c1b985ca6f32f7e84549b45d0681fbf33e98567c76af6'
  );
  RunTestCTR(
    '--- Testing AES-256 CTR: Empty Data ---',
    '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
    '',
    ''
  );
  WriteLn('----- GCM -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('Nonce: ', LowerCase(IVToHex(IV)));
  RunTestGCM(
    '--- Testing AES-256 GCM: No Extra Auth Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    '6bc1bee22e409f96e93d7e117393172a',
    '2b7a47746822e658dedeb57b0fe04a37',
    ''
  );
  RunTestGCM(
    '--- Testing AES-256 GCM: Extra Auth Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',
    '6bc1bee22e409f96e93d7e117393172a',
    '2b7a47746822e658dedeb57b0fe04a37',
    '192.168.1.123'
  );
end;

procedure Run;
begin
  RunAESTests;
  //ReadLn;
end;

begin
  Run;
end.

