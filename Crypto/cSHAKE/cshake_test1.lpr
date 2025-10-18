program cshake_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

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
procedure TestCSHAKE_128;
  const HashLen = 32;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUInt8Array;
    var Actual: String;
  begin
    Digest := UcSHAKE_128(Data, HashLen, 'cSHAKE', '128');
    Actual := DigestToHexString(Digest);
    WriteLn(Name);
    WriteSuccessOrFail(Actual, Expected);
    WriteLn('Actual:   ', Actual);
    WriteLn('Expected: ', Expected);
    WriteLn;
  end;
begin
  WriteLn('cSHAKE-128 Test Results:');
  WriteLn('====================');
  Test(
    'Empty string',
    '',
    '5d9d667b166e43c11d688e425907733f8b32d0575c35c7123f450ce9ffffb4a7'
  );
  Test(
    'String "abc"',
    'abc',
    '2b321e56a13c2820b4b18c63d191712cc2af511f6390d7efec2d294f74ef9ba7'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    '88ffd257d060b4f06880ced2997098a2d27e2276f10321512059fe7a59cd9ec6'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    '23859d3f63178af742aff440e13913527500ed532fbb30456443768c2aff54c7'
  );
end;

procedure TestCSHAKE_256;
  const HashLen = 64;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUInt8Array;
    var Actual: String;
  begin
    Digest := UcSHAKE_256(Data, HashLen, 'cSHAKE', '256');
    Actual := DigestToHexString(Digest);
    WriteLn(Name);
    WriteSuccessOrFail(Actual, Expected);
    WriteLn('Actual:   ', Actual);
    WriteLn('Expected: ', Expected);
    WriteLn;
  end;
begin
  WriteLn('cSHAKE-256 Test Results:');
  WriteLn('====================');
  Test(
    'Empty string',
    '',
    'a3d69193b5561aed0fbe9ae7ffb7861c6769c949727f6a26d5b0f330f13b12d1779f80bcc2afe8e501ded7b439c31a113a7632b97e6e79422b70e940254dd30d'
  );
  Test(
    'String "abc"',
    'abc',
    '891c42f7358386c03581d4321be5660213e75da90a7d6405d1f266a6a6767bc4cfc4457f5b87ea65c2fb3cc7df821aa08e49a388009acfdcb1dba49059654c09'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    '72ba08e8fa7b0a2c1dc0ab4dcb3ef3442813b9343c66fcb66b80a6b25e28fdfd7afe742b75e6eb485a53380aa0ca0160b3ebc9248b694e695ddf38fd57e78f3d'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    '9fd5f6a619aadc02ca241a737f9ed97b65fa377f4d11925fbb56d2833e2ce3fdde2aa3fb0ed84217c9b12f019348ebf165b3873c20b3ed659c1d48f2c9ec8189'
  );
end;

procedure TestcSHAKE;
  var Hash: TUInt8Array;
  const HashLen = 64;
  const Data = 'Hello, World!';
  const FN = 'cSHAKE';
  const C = '256';
begin
  Hash := UcSHAKE_256(Data, HashLen, FN, C);
  WriteLn(LowerCase(UBytesToHex(Hash)));
  WriteLn(Length(Hash));
  WriteLn('72ba08e8fa7b0a2c1dc0ab4dcb3ef3442813b9343c66fcb66b80a6b25e28fdfd7afe742b75e6eb485a53380aa0ca0160b3ebc9248b694e695ddf38fd57e78f3d');
end;

begin
  //TestcSHAKE;
  TestCSHAKE_128;
  TestcSHAKE_256;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

