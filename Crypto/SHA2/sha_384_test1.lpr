program sha_384_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function StringToUInt8Array(const s: String): TUInt8Array;
  var i: Int32;
begin
  Result := nil;
  SetLength(Result, Length(s));
  for i := 0 to High(Result) do
  begin
    Result[i] := Ord(s[i + 1]);
  end;
end;

function DigestToHexString(const Digest: TUDigestSHA2_384): String;
  var i: Int32;
begin
  Result := '';
  for i := 0 to High(Digest) do
  begin
    Result += LowerCase(IntToHex(Digest[i], 2));
  end;
end;

// Test function
procedure TestSHA384;
  procedure WriteSuccessOrFail(const a, b: String);
  begin
    if a = b then
    begin
      WriteLn('SUCCESS');
    end
    else
    begin
      WriteLn('FAIL');
    end;
  end;
  var Digest: TUDigestSHA2_384;
  var Actual, Expected: String;
begin
  WriteLn('SHA-384 Test Results:');
  WriteLn('====================');

  // Test 1: Empty string
  Digest := USHA2_384('');
  Actual := DigestToHexString(Digest);
  Expected := '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b';
  WriteLn('Empty string');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;

  // Test 2: "abc"
  Digest := USHA2_384('abc');
  Actual := DigestToHexString(Digest);
  Expected := 'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7';
  WriteLn('String "abc"');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;

  // Test 3: "Hello, World!"
  Digest := USHA2_384('Hello, World!');
  Actual := DigestToHexString(Digest);
  Expected := '5485cc9b3365b4305dfb4e8337e0a598a574f8242bf17289e0dd6c20a3cd44a089de16ab4ab308f63e44b1170eb5f515';
  WriteLn('String "Hello, World!"');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;

  // Test 4: Longer string
  Digest := USHA2_384('The quick brown fox jumps over the lazy dog');
  Actual := DigestToHexString(Digest);
  Expected := 'ca737f1014a48f4c0b6dd43cb177b0afd9e5169367544c494011e3317dbf9a509cb1e5dc1e85a941bbee3d7f2afbc9b1';
  WriteLn('String "The quick brown fox jumps over the lazy dog"');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;
end;

begin
  TestSHA384;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

