program sha1_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

procedure Run;
  var Digest: TUSHA1Digest;
begin
  Digest := USHA1('Hello World!');
  WriteLn('Digest:   ', LowerCase(UBytesToHex(Digest)));
  WriteLn('Expected: ', '2ef7bde608ce5404e97d5f042f95f89f1c232871');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

