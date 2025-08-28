program aes_192_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function BytesToKey(const Bytes: TUInt8Array): TUAES.TKey192;
  var n: Int32;
begin
  n := UMin(Length(Bytes), Length(TUAES.TKey192));
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
    var Key: TUAES.TKey192;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_PKCS7_ECB_192(Data, Key);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_PKCS7_ECB_192(Cipher, Key);
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
    var Key: TUAES.TKey192;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_PKCS7_CBC_192(Data, Key, IV);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_PKCS7_CBC_192(Cipher, Key, IV);
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
    var Key: TUAES.TKey192;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_CTR_192(Data, Key, IV);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_CTR_192(Cipher, Key, IV);
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
    var Key: TUAES.TKey192;
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
    Cipher := UEncrypt_AES_GCM_192(Data, Key, IV, AAD, AuthTag);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_GCM_192(Cipher, Key, IV, AAD, DecryptTag);
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
    '--- Testing AES-192 ECB: Single Block ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    '6bc1bee22e409f96e93d7e117393172a',
    '5d2a8775fd675ae06daf48b97441d5e2af92459629e8e9c27a79edb00d04f802'
  );
  RunTestECB(
    '--- Testing AES-192 ECB: Multi-Block & Padding ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '11273082687c9cd122fc826f12aeebdc457cf6844b0d204e9fc010e2b41347dcaf92459629e8e9c27a79edb00d04f802'
  );
  RunTestECB(
    '--- Testing AES-192 ECB: Empty Data ---',
    '0123456789abcdef0123456789abcdef0123456789abcdef',
    '',
    '0f1f3b1cef61325a2cb80a3e061fcf65'
  );
  WriteLn('----- CBC -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('IV: ', LowerCase(IVToHex(IV)));
  RunTestCBC(
    '--- Testing AES-192 CBC: Single Block ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    '6bc1bee22e409f96e93d7e117393172a',
    '8b68ffcf6fb82e5c8c415f83d93cbfb9f9bf3945490f2a34d98226932b02d68b'
  );
  RunTestCBC(
    '--- Testing AES-192 CBC: Multi-Block & Padding ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '09eaeed06c994fcc64ec2fa2991da1f1f7364f00026b981d61b4fbdabbb55455ed57f76379f5521b0f9aa0f98485bccc'
  );
  RunTestCBC(
    '--- Testing AES-192 CBC: Empty Data ---',
    '0123456789abcdef0123456789abcdef0123456789abcdef',
    '',
    '619b9a3973eea5f2ef2e4833c631a06a'
  );
  WriteLn('----- CTR -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('Nonce: ', LowerCase(IVToHex(IV)));
  RunTestCTR(
    '--- Testing AES-192 CTR: Short Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    '6bc1bee22e409f96e93d7e117393172a',
    '5a8c82134cc0ea1b3607fb042240624a'
  );
  RunTestCTR(
    '--- Testing AES-192 CTR: Longer Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '9f60b6a67c83d911418deab9147cfb31874616ed4f0bb5bfe5c98e17fa718cbe'
  );
  RunTestCTR(
    '--- Testing AES-192 CTR: Empty Data ---',
    '0123456789abcdef0123456789abcdef0123456789abcdef',
    '',
    ''
  );
  WriteLn('----- GCM -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('Nonce: ', LowerCase(IVToHex(IV)));
  RunTestGCM(
    '--- Testing AES-192 GCM: No Extra Auth Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    '6bc1bee22e409f96e93d7e117393172a',
    '83a5fffadde272d61f96807580e40106',
    ''
  );
  RunTestGCM(
    '--- Testing AES-192 GCM: Extra Auth Data ---',
    '603deb1015ca71be2b73aef0857d77811f352c073b6108d7',
    '6bc1bee22e409f96e93d7e117393172a',
    '83a5fffadde272d61f96807580e40106',
    '192.168.1.123'
  );
end;

begin
  RunAESTests;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

