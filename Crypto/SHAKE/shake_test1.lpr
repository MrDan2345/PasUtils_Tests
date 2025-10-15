program shake_test1;

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

function DigestToHexString(const Digest: array of UInt8): String;
  var i: Int32;
begin
  Result := '';
  for i := 0 to High(Digest) do
  begin
    Result += LowerCase(IntToHex(Digest[i], 2));
  end;
end;

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

// Test functions
procedure TestSHAKE_128;
  const HashLen = 32;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUInt8Array;
    var Actual: String;
  begin
    Digest := USHAKE_128(Data, HashLen);
    Actual := DigestToHexString(Digest);
    WriteLn(Name);
    WriteSuccessOrFail(Actual, Expected);
    WriteLn('Actual:   ', Actual);
    WriteLn('Expected: ', Expected);
    WriteLn;
  end;
begin
  WriteLn('SHA3-224 Test Results:');
  WriteLn('====================');
  Test(
    'Empty string',
    '',
    '7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef26'
  );
  Test(
    'String "abc"',
    'abc',
    '5881092dd818bf5cf8a3ddb793fbcba74097d5c526a6d35f97b83351940f2cc8'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    '2bf5e6dee6079fad604f573194ba8426bd4d30eb13e8ba2edae70e529b570cbd'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    'f4202e3c5852f9182a0430fd8144f0a74b95e7417ecae17db0f8cfeed0e3e66e'
  );
end;

procedure TestSHAKE_256;
  const HashLen = 64;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUInt8Array;
    var Actual: String;
  begin
    Digest := USHAKE_256(Data, HashLen);
    Actual := DigestToHexString(Digest);
    WriteLn(Name);
    WriteSuccessOrFail(Actual, Expected);
    WriteLn('Actual:   ', Actual);
    WriteLn('Expected: ', Expected);
    WriteLn;
  end;
begin
  WriteLn('SHA3-256 Test Results:');
  WriteLn('====================');
  Test(
    'Empty string',
    '',
    '46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762fd75dc4ddd8c0f200cb05019d67b592f6fc821c49479ab48640292eacb3b7c4be'
  );
  Test(
    'String "abc"',
    'abc',
    '483366601360a8771c6863080cc4114d8db44530f8f1e1ee4f94ea37e78b5739d5a15bef186a5386c75744c0527e1faa9f8726e462a12a4feb06bd8801e751e4'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    'b3be97bfd978833a65588ceae8a34cf59e95585af62063e6b89d0789f372424e8b0d1be4f21b40ce5a83a438473271e0661854f02d431db74e6904d6c347d757'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    '2f671343d9b2e1604dc9dcf0753e5fe15c7c64a0d283cbbf722d411a0e36f6ca1d01d1369a23539cd80f7c054b6e5daf9c962cad5b8ed5bd11998b40d5734442'
  );
end;

procedure TestSHAKE_2561;
  var Hash: TUInt8Array;
  const HashLen = 64;
  const Data = 'Hello World!';
begin
  Hash := USHAKE_256(UStrToBytes(Data), HashLen);
  WriteLn(LowerCase(UBytesToHex(Hash)));
  WriteLn(Length(Hash));
  WriteLn('35259d2903a1303d3115c669e2008510fc79acb50679b727ccb567cc3f786de3553052e47d4dd715cc705ce212a92908f4df9e653fa3653e8a7855724d366137');
end;

begin
  TestSHAKE_128;
  TestSHAKE_256;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

