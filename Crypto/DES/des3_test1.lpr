program des3_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function HexToKey(const Hex: String): TUDES.TKey3;
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
  for i := n to High(Result) do
  begin
    Result[i] := 0;
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

procedure TestDES_ECB;
  var Key: TUDES.TKey3;
  var DataStr, DecryptStr: String;
  var Data, Decrypt: TUInt8Array;
  var Cipher: TUInt8Array;
begin
  WriteLn('----- Test ECB -----');
  Key := HexToKey('3c47d1aa629f57a38194cbbc9de20183b54f62a5868a663f');
  WriteLn('Key: ', LowerCase(UBytesToHex(Key)));
  DataStr := 'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef';
  Data := UHexToBytes(DataStr);
  Cipher := UEncrypt_DES_Triple_PKCS7_ECB(Data, Key);
  Decrypt := UDecrypt_DES_Triple_PKCS7_ECB(Cipher, Key);
  DecryptStr := LowerCase(UBytesToHex(Decrypt));
  WriteLn('Data: ', DataStr);
  WriteLn('Cipher:   ', LowerCase(UBytesToHex(Cipher)));
  WriteLn('Expected: ', '793c077570138f8a17f275a3f166b5bb8260d2ae0d6bf25856d24966bfd6878e912a91fc5bab4b3d');
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
  var IV: TUDES.TInitVector;
  var Key: TUDES.TKey3;
  var DataStr, DecryptStr: String;
  var Data, Decrypt: TUInt8Array;
  var Cipher: TUInt8Array;
begin
  WriteLn('----- Test CBC -----');
  IV := HexToIV('3c47d1aa629f57a3');
  Key := HexToKey('3c47d1aa629f57a38194cbbc9de20183b54f62a5868a663f');
  WriteLn('Key: ', LowerCase(UBytesToHex(Key)));
  DataStr := 'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef';
  Data := UHexToBytes(DataStr);
  Cipher := UEncrypt_DES_Triple_PKCS7_CBC(Data, Key, IV);
  Decrypt := UDecrypt_DES_Triple_PKCS7_CBC(Cipher, Key, IV);
  DecryptStr := LowerCase(UBytesToHex(Decrypt));
  WriteLn('Data: ', DataStr);
  WriteLn('Cipher:   ', LowerCase(UBytesToHex(Cipher)));
  WriteLn('Expected: ', '532992039dde638b6cdbfbc69448881b69cc02274c2b85800a376668cc0b3db2329512df5d0fcb43');
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

procedure TestDES_CTR;
  var IV: TUDES.TInitVector;
  var Key: TUDES.TKey3;
  var DataStr, DecryptStr: String;
  var Data, Decrypt: TUInt8Array;
  var Cipher: TUInt8Array;
begin
  WriteLn('----- Test CTR -----');
  IV := HexToIV('3c47d1aa629f57a3');
  Key := HexToKey('3c47d1aa629f57a38194cbbc9de20183b54f62a5868a663f');
  WriteLn('Key: ', LowerCase(UBytesToHex(Key)));
  DataStr := 'ae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52ef';
  Data := UHexToBytes(DataStr);
  Cipher := UEncrypt_DES_Triple_PKCS7_CTR(Data, Key, IV);
  Decrypt := UDecrypt_DES_Triple_PKCS7_CTR(Cipher, Key, IV);
  DecryptStr := LowerCase(UBytesToHex(Decrypt));
  WriteLn('Data: ', DataStr);
  WriteLn('Cipher:   ', LowerCase(UBytesToHex(Cipher)));
  WriteLn('Expected: ', '66386e484012c6caf18f3db42da78eefc9c63b993de59cedf74f1ebb796e7ed7');
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

begin
  TestDES_ECB;
  TestDES_CBC;
  TestDES_CTR;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

