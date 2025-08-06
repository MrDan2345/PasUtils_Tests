program md5_test1;

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
  var Digest: TUMD5Digest;
begin
  Digest := UMD5('Hello World!');
  WriteLn('Digest:   ', LowerCase(UBytesToHex(Digest)));
  WriteLn('Expected: ', 'ed076287532e86365e841e92bfc50d8c');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

