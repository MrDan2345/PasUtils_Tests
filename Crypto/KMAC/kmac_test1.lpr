program kmac_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

procedure Test;
  const MessageStr = 'Hello World!';
  const KeyLong = '3c47d1aa629f57a38194cbbc9de20183b54f62a5868a663f0343fb9c732257055e60fda09c2cfa90391598d0152a186e4b842328e672099d6b7b0a7c29d92574784648cd3922';
  const KeyShort = '48c73398c61b0cbc92baddec1ff40e12a68c';
  const CustomizationStr = 'test';
  var Key: TUInt8Array;
  var Customization: TUInt8Array;
  var Message: TUInt8Array;
  var Digest: TUInt8Array;
begin
  Key := UStrToBytes(KeyLong);
  Customization := UStrToBytes(CustomizationStr);
  Message := UStrToBytes(MessageStr);
  Digest := UKMAC_256(Message, 64, Key, Customization);
  WriteLn('Digest LK: ', LowerCase(UBytesToHex(Digest)));
  WriteLn('Expected:  ', '46475c1dab5a7e34c03c8f08eeefd44ab5d5bc6ed7c3f458931eb8d584d15c5c33dec232c2d56cfbe44f2784d406508f1ec7184dd46fc9b3335b2b40d77dcc42');
  Key := UStrToBytes(KeyShort);
  Digest := UKMAC_128(Message, 32, Key, Customization);
  WriteLn('Digest SK: ', LowerCase(UBytesToHex(Digest)));
  WriteLn('Expected:  ', '40a3405ebbefa9881a8577b2326dbd8c9e37e3003e9952a85eb4bc92cde4c122');
end;

begin
  Test;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

