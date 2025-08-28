program sha3_test1;

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
procedure TestSHA3_224;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUDigestSHA3_224;
    var Actual: String;
  begin
    Digest := USHA3_224(Data);
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
    '6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7'
  );
  Test(
    'String "abc"',
    'abc',
    'e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    '853048fb8b11462b6100385633c0cc8dcdc6e2b8e376c28102bc84f2'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    'd15dadceaa4d5d7bb3b48f446421d542e08ad8887305e28d58335795'
  );
end;

procedure TestSHA3_256;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUDigestSHA3_256;
    var Actual: String;
  begin
    Digest := USHA3_256(Data);
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
    'a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a'
  );
  Test(
    'String "abc"',
    'abc',
    '3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    '1af17a664e3fa8e419b8ba05c2a173169df76162a5a286e0c405b460d478f7ef'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    '69070dda01975c8c120c3aada1b282394e7f032fa9cf32f4cb2259a0897dfc04'
  );
end;

procedure TestSHA3_384;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUDigestSHA3_384;
    var Actual: String;
  begin
    Digest := USHA3_384(Data);
    Actual := DigestToHexString(Digest);
    WriteLn(Name);
    WriteSuccessOrFail(Actual, Expected);
    WriteLn('Actual:   ', Actual);
    WriteLn('Expected: ', Expected);
    WriteLn;
  end;
begin
  WriteLn('SHA3-384 Test Results:');
  WriteLn('====================');
  Test(
    'Empty string',
    '',
    '0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004'
  );
  Test(
    'String "abc"',
    'abc',
    'ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    'aa9ad8a49f31d2ddcabbb7010a1566417cff803fef50eba239558826f872e468c5743e7f026b0a8e5b2d7a1cc465cdbe'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    '7063465e08a93bce31cd89d2e3ca8f602498696e253592ed26f07bf7e703cf328581e1471a7ba7ab119b1a9ebdf8be41'
  );
end;

procedure TestSHA3_512;
  procedure Test(const Name: String; const Data: String; const Expected: String);
    var Digest: TUDigestSHA3_512;
    var Actual: String;
  begin
    Digest := USHA3_512(Data);
    Actual := DigestToHexString(Digest);
    WriteLn(Name);
    WriteSuccessOrFail(Actual, Expected);
    WriteLn('Actual:   ', Actual);
    WriteLn('Expected: ', Expected);
    WriteLn;
  end;
begin
  WriteLn('SHA3-512 Test Results:');
  WriteLn('====================');
  Test(
    'Empty string',
    '',
    'a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26'
  );
  Test(
    'String "abc"',
    'abc',
    'b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0'
  );
  Test(
    'String "Hello, World!"',
    'Hello, World!',
    '38e05c33d7b067127f217d8c856e554fcff09c9320b8a5979ce2ff5d95dd27ba35d1fba50c562dfd1d6cc48bc9c5baa4390894418cc942d968f97bcb659419ed'
  );
  Test(
    'String "The quick brown fox jumps over the lazy dog"',
    'The quick brown fox jumps over the lazy dog',
    '01dedd5de4ef14642445ba5f5b97c15e47b9ad931326e4b0727cd94cefc44fff23f07bf543139939b49128caf436dc1bdee54fcb24023a08d9403f9b4bf0d450'
  );
end;

begin
  TestSHA3_224;
  TestSHA3_256;
  TestSHA3_384;
  TestSHA3_512;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

