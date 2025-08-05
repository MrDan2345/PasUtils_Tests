program signing_test;

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
  var Signature: TUInt8Array;
  var Data: TUInt8Array;
  var Key: TURSA.TKey;
begin
  WriteLn('Generating Key...');
  Key := UMakeRSAKey(1024, 24);
  Data := UStrToBytes('Hello World!');
  Signature := USign_SHA256(Data, Key);
  WriteLn('Signature: ', UBytesToHex(Signature));
  WriteLn('Verify: ', UBoolToStr(UVerify(Data, Signature, Key)));
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

