program des_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function HexToKey(const Hex: String): TUDES.TKey;
  var Bytes: TUInt8Array;
  var i, n: Int32;
begin
  UClear(Result, SizeOf(Result));
  Bytes := UHexToBytes(Hex);
  n := UMin(Length(Result), Length(Bytes));
  for i := 0 to n - 1 do
  begin
    Result[i] := Bytes[i];
  end;
end;

function HexToIV(const Hex: String): TUDES.TInitVector;
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

function IVToHex(const IV: TUDES.TInitVector): String;
  var i: Int32;
begin
  Result := '';
  for i := 0 to High(IV) do
  begin
    Result += IntToHex(IV[i], 2);
  end;
end;

procedure TestDES_CTR;
  var Key: TUDES.TKey;
  var IV: TUDES.TInitVector;
  var DataStr, DecryptStr: String;
  var Data, Decrypt: TUInt8Array;
  var Cipher: TUInt8Array;
begin
  WriteLn('----- Test CTR -----');
  Key := HexToKey('da39a3ee5e6b4b0d');
  IV := HexToIV('3c47d1aa629f57a3');
  WriteLn('Key: ', LowerCase(UBytesToHex(Key)));
  WriteLn('IV:  ', LowerCase(IVToHex(IV)));
  DataStr := 'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef';
  Data := UHexToBytes(DataStr);
  Cipher := UEncrypt_DES_PKCS7_CTR(Data, Key, IV);
  Decrypt := UDecrypt_DES_PKCS7_CTR(Cipher, Key, IV);
  DecryptStr := LowerCase(UBytesToHex(Decrypt));
  WriteLn('Data: ', DataStr);
  WriteLn('Cipher:   ', LowerCase(UBytesToHex(Cipher)));
  WriteLn('Expected: ', '8517dbb708c51d22c81c5bf150b2f440d9cc001b7659d16be238def293c970bf');
  WriteLn('Decrypt: ', DecryptStr);
  if DataStr = DecryptStr then
  begin
    WriteLn('Decrypt SUCCESS');
  end
  else
  begin
    WriteLn('Decrypt FAIL');
  end;
  WriteLn;
end;

procedure TestDES_CBC;
  var Key: TUDES.TKey;
  var IV: TUDES.TInitVector;
  var DataStr, DecryptStr: String;
  var Data, Decrypt: TUInt8Array;
  var Cipher: TUInt8Array;
begin
  WriteLn('----- Test CBC -----');
  Key := HexToKey('da39a3ee5e6b4b0d');
  IV := HexToIV('3c47d1aa629f57a3');
  WriteLn('Key: ', LowerCase(UBytesToHex(Key)));
  WriteLn('IV:  ', LowerCase(IVToHex(IV)));
  DataStr := 'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef';
  Data := UHexToBytes(DataStr);
  Cipher := UEncrypt_DES_PKCS7_CBC(Data, Key, IV);
  Decrypt := UDecrypt_DES_PKCS7_CBC(Cipher, Key, IV);
  DecryptStr := LowerCase(UBytesToHex(Decrypt));
  WriteLn('Data: ', DataStr);
  WriteLn('Cipher:   ', LowerCase(UBytesToHex(Cipher)));
  WriteLn('Expected: ', '4f7f0c6ac2729a92df69e5ef1439e34099485e4c0346e12b91e7ae56de4654fd676467207f1f6edc');
  WriteLn('Decrypt: ', DecryptStr);
  if DataStr = DecryptStr then
  begin
    WriteLn('Decrypt SUCCESS');
  end
  else
  begin
    WriteLn('Decrypt FAIL');
  end;
  WriteLn;
end;

procedure TestDES_ECB;
  var Key: TUDES.TKey;
  var DataStr, DecryptStr: String;
  var Data, Decrypt: TUInt8Array;
  var Cipher: TUInt8Array;
begin
  WriteLn('----- Test ECB -----');
  Key := HexToKey('da39a3ee5e6b4b0d');
  WriteLn('Key: ', LowerCase(UBytesToHex(Key)));
  DataStr := 'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef';
  Data := UHexToBytes(DataStr);
  Cipher := UEncrypt_DES_PKCS7_ECB(Data, Key);
  Decrypt := UDecrypt_DES_PKCS7_ECB(Cipher, Key);
  DecryptStr := LowerCase(UBytesToHex(Decrypt));
  WriteLn('Data: ', DataStr);
  WriteLn('Cipher:   ', LowerCase(UBytesToHex(Cipher)));
  WriteLn('Expected: ', '86a746d95fd708b4a45bea73c81d97c22f89e226a0de214f968e7f374205ea5c1f0ad25e8ea94e78');
  WriteLn('Decrypt: ', DecryptStr);
  if DataStr = DecryptStr then
  begin
    WriteLn('Decrypt SUCCESS');
  end
  else
  begin
    WriteLn('Decrypt FAIL');
  end;
  WriteLn;
end;

procedure Run;
begin
  TestDES_ECB;
  TestDES_CBC;
  TestDES_CTR;
{$if defined(windows)}
  ReadLn;
{$endif}
end;

begin
  Run;
end.

