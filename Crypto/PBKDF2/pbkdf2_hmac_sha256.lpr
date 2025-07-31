program pbkdf2_hmac_sha256;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

function GenPassword(const PassLength: Int32): String;
  const Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!"#$%&''()*+,-./:;<=>?@[\]^_{|}~';
  var i: Int32;
begin
  UThreadRandomize;
  Result := '';
  SetLength(Result, PassLength);
  for i := 1 to PassLength do
  begin
    Result[i] := AnsiChar(Chars[UThreadRandom(Length(Chars)) + 1]);
  end;
end;

procedure Run;
  const PasswordStr = 'WtWhCaIUIDITWMB=KY_<nC2';
  const SaltStr = '89d3db776b20228c61b506423ca105c2';
  const SaltB64 = 'idPbd2sgIoxhtQZCPKEFwg==';
  const Iterations = 20 * 1000;
  var Password, Salt: TUInt8Array;
  var Digest: TUInt8Array;
begin;
  Password := UStrToBytes(PasswordStr);
  Salt := UHexToBytes(SaltStr);
  WriteLn('Generating hash with ', Iterations, ' Iterations...');
  Digest := UPBKDF2_HMAC_SHA256(Password, Salt, SizeOf(TUAES.TKey256), Iterations);
  WriteLn('Password: ', PasswordStr);
  WriteLn('Salt (Hex): ', SaltStr);
  WriteLn('Salt (B64): ', SaltB64);
  WriteLn('Digest (Hex): ', UBytesToHex(Digest));
  WriteLn('Digest (B64):   ', UBytesToBase64(Digest));
  WriteLn('Expected (B64): ', 'gHn0OFf58FJES7/UbFK3ikuz18fgES7kYSSwkaCt5ok=');
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

