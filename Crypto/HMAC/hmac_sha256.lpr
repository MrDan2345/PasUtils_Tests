program hmac_sha256;

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
  var Key: TUInt8Array;
  var Message: TUInt8Array;
  var Digest: TUDigestSHA2_256;
begin
  Key := UStrToBytes(KeyLong);
  Message := UStrToBytes(MessageStr);
  Digest := UHMAC_SHA2_256(Key, Message);
  WriteLn('Digest LK: ', LowerCase(UBytesToHex(Digest)));
  WriteLn('Expected:  ', '2c9783226b16ef4f883f80dde1f2e2627957b1463be3e5a91686a8b338739b09');
  Key := UStrToBytes(KeyShort);
  Digest := UHMAC_SHA2_256(Key, Message);
  WriteLn('Digest SK: ', LowerCase(UBytesToHex(Digest)));
  WriteLn('Expected:  ', 'c688254af4aeb274d69409055201229d80a4fcf8586e5ab7746fa34a1f1b451a');
end;

begin
  Test;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

