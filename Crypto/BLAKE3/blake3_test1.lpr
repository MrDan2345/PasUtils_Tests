program blake3_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

procedure Test;
  var Data: TUInt8Array;
  var Hash: TUInt8Array;
  var Key: TUBLAKE3.TKey;
  const KDFContext = 'my test context';
  const KDFPassword = 'my password';
begin
  Data := UStrToBytes('Hello, World!');
  Key := TUBLAKE3.KeyFromHex('8c97b7d89adb8bd86c9fa562704ce40ef645627acacf877a9164ecd6125616a5');
  Hash := UBLAKE3_Hash(Data, 32);
  WriteLn('Data: ', UBytesToString(Data));
  WriteLn('Hash:');
  WriteLn('Actual:   ', LowerCase(UBytesToHex(Hash)));
  WriteLn('Expected: 288a86a79f20a3d6dccdca7713beaed178798296bdfa7913fa2a62d9727bf8f8');
  Hash := UBLAKE3_Hash(Data, Key, 32);
  WriteLn('Keyed Hash:');
  WriteLn('Key: ', LowerCase(UBytesToHex(Key)));
  WriteLn('Actual:   ', LowerCase(UBytesToHex(Hash)));
  WriteLn('Expected: c1441c8e90612f159d06fb08ff7babc92d7233b310955eec25960edeca04b1d1');
  Hash := UBLAKE3_KDF(UStrToBytes(KDFContext), UStrToBytes(KDFPassword), 32);
  WriteLn('KDF:');
  WriteLn('Actual:   ', LowerCase(UBytesToHex(Hash)));
  WriteLn('Expected: 51844184a9338b30f111cf890e55347e83bafe36253d929ec3f37833bd5958bd');
end;

begin
  Test;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

