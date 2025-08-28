program aes_128_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function BytesToKey(const Bytes: TUInt8Array): TUAES.TKey128;
  var n: Int32;
begin
  n := UMin(Length(Bytes), Length(TUAES.TKey128));
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
    var Key: TUAES.TKey128;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_PKCS7_ECB_128(Data, Key);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_PKCS7_ECB_128(Cipher, Key);
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
    var Key: TUAES.TKey128;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_PKCS7_CBC_128(Data, Key, IV);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_PKCS7_CBC_128(Cipher, Key, IV);
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
    var Key: TUAES.TKey128;
    var Data, Cipher, Decrypt: TUInt8Array;
    var CipherStr, DecryptStr: String;
  begin
    WriteLn(Name);
    Key := BytesToKey(UHexToBytes(KeyStr));
    Data := UHexToBytes(DataHex);
    Cipher := UEncrypt_AES_CTR_128(Data, Key, IV);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_CTR_128(Cipher, Key, IV);
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
    var Key: TUAES.TKey128;
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
    Cipher := UEncrypt_AES_GCM_128(Data, Key, IV, AAD, AuthTag);
    CipherStr := UBytesToHex(Cipher);
    Decrypt := UDecrypt_AES_GCM_128(Cipher, Key, IV, AAD, DecryptTag);
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
    '--- Testing AES-128 ECB: Single Block ---',
    '603deb1015ca71be2b73aef0857d7781',
    '6bc1bee22e409f96e93d7e117393172a',
    '8d91589bea81105cdd0c451545d0630c7db96f08e49b29e2aa359184a173b21d'
  );
  RunTestECB(
    '--- Testing AES-128 ECB: Multi-Block & Padding ---',
    '603deb1015ca71be2b73aef0857d7781',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    'b4693a3acaa04b978d632db14fd849dcda35ca29c7fd0cd1a6a427e17d1860157db96f08e49b29e2aa359184a173b21d'
  );
  RunTestECB(
    '--- Testing AES-128 ECB: Empty Data ---',
    '0123456789abcdef0123456789abcdef',
    '',
    '0efb6bfed93b4d1ea2123ba4db075ff6'
  );
  WriteLn('----- CBC -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('IV: ', LowerCase(IVToHex(IV)));
  RunTestCBC(
    '--- Testing AES-128 CBC: Single Block ---',
    '603deb1015ca71be2b73aef0857d7781',
    '6bc1bee22e409f96e93d7e117393172a',
    'ae653a074e6aff025ec5275a318864a3ab60f0e6b517efeebf650125a4a9d641'
  );
  RunTestCBC(
    '--- Testing AES-128 CBC: Multi-Block & Padding ---',
    '603deb1015ca71be2b73aef0857d7781',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '254738d146640d40c2e70cd6927a31e64dbb6ecea5434d79cd9dca56aa220c252ce305ae124f407f2cfc24cd0b1d70a9'
  );
  RunTestCBC(
    '--- Testing AES-128 CBC: Empty Data ---',
    '0123456789abcdef0123456789abcdef',
    '',
    '78322d69c129cbc91fbfefb05ecea156'
  );
  WriteLn('----- CTR -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('Nonce: ', LowerCase(IVToHex(IV)));
  RunTestCTR(
    '--- Testing AES-128 CTR: Short Data ---',
    '603deb1015ca71be2b73aef0857d7781',
    '6bc1bee22e409f96e93d7e117393172a',
    '45b656437285fee5b1f845bd1f2b9567'
  );
  RunTestCTR(
    '--- Testing AES-128 CTR: Longer Data ---',
    '603deb1015ca71be2b73aef0857d7781',
    'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef',
    '805a62f642c6cdefc672540029170c1caa02fc6e2fd5b5b2a91249c70d931065'
  );
  RunTestCTR(
    '--- Testing AES-128 CTR: Empty Data ---',
    '0123456789abcdef0123456789abcdef',
    '',
    ''
  );
  WriteLn('----- GCM -----');
  IV := HexToIV('8c97b7d89adb8bd86c9fa562704ce40e');
  WriteLn('Nonce: ', LowerCase(IVToHex(IV)));
  RunTestGCM(
    '--- Testing AES-128 GCM: No Extra Auth Data ---',
    '603deb1015ca71be2b73aef0857d7781',
    '6bc1bee22e409f96e93d7e117393172a',
    'd428089c2a5d0168d47bb168245de641',
    ''
  );
  RunTestGCM(
    '--- Testing AES-128 GCM: Extra Auth Data ---',
    '603deb1015ca71be2b73aef0857d7781',
    '6bc1bee22e409f96e93d7e117393172a',
    'd428089c2a5d0168d47bb168245de641',
    '192.168.1.123'
  );
end;

begin
  RunAESTests;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

