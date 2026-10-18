program sha512_test1;

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
  var Digest: TUDigestSHA2_512;
begin
  Digest := USHA2_512('Hello World!');
  WriteLn('Digest:   ', LowerCase(UBytesToHex(Digest)));
  WriteLn('Expected: ', '861844d6704e8573fec34d967e20bcfef3d424cf48be04e6dc08f2bd58c729743371015ead891cc3cf1c9d34b49264b510751b1ff9e537937bc46b5d6ff4ecc8');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

