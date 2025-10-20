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
  var Hash: TUInt8Array;
begin
  Hash := UBLAKE3_Hash(UStrToBytes('Hello, World!'), 32);
  WriteLn('Actual:   ', LowerCase(UBytesToHex(Hash)));
  WriteLn('Expected: 288a86a79f20a3d6dccdca7713beaed178798296bdfa7913fa2a62d9727bf8f8');
end;

begin
  Test;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

