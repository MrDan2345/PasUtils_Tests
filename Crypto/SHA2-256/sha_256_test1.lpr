program sha_256_test1;

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

function DigestToHexString(const Digest: TUSHA2_256Digest): String;
  var i: Int32;
begin
  Result := '';
  for i := 0 to High(Digest) do
  begin
    Result += LowerCase(IntToHex(Digest[i], 2));
  end;
end;

// Test function
procedure TestSHA256;
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
  var Digest: TUSHA2_256Digest;
  var Actual, Expected: String;
begin
  WriteLn('SHA-256 Test Results:');
  WriteLn('====================');

  // Test 1: Empty string
  Digest := USHA2_256('');
  Actual := DigestToHexString(Digest);
  Expected := 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855';
  WriteLn('Empty string');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;

  // Test 2: "abc"
  Digest := USHA2_256('abc');
  Actual := DigestToHexString(Digest);
  Expected := 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad';
  WriteLn('String "abc"');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;

  // Test 3: "Hello, World!"
  Digest := USHA2_256('Hello, World!');
  Actual := DigestToHexString(Digest);
  Expected := 'dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f';
  WriteLn('String "Hello, World!"');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;

  // Test 4: Longer string
  Digest := USHA2_256('The quick brown fox jumps over the lazy dog');
  Actual := DigestToHexString(Digest);
  Expected := 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592';
  WriteLn('String "The quick brown fox jumps over the lazy dog"');
  WriteSuccessOrFail(Actual, Expected);
  WriteLn('Actual:   ', Actual);
  WriteLn('Expected: ', Expected);
  WriteLn;
end;

begin
  TestSHA256;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

