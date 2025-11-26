program hkdf_test1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

procedure PassedFailed(const Cond: Boolean; const Name: String);
begin
  if Cond then
  begin
    WriteLn('✅ ', Name, ' PASSED!')
  end
  else
  begin
    WriteLn('❌ ', Name, ' FAILED!');
  end;
  WriteLn;
end;

procedure Test_SHA2_256(
  const IKM, Salt, Info, ExpectedPRK, ExpectedOKM: TUInt8Array;
  const HashLength: UInt32;
  const TestName: String
);
  var PRK, OKM: TUInt8Array;
begin
  WriteLn('Testing ', TestName);

  PRK := UHKDF_Extract_SHA2_256(Salt, IKM);
  WriteLn('Expected PRK: ', UBytesToHexLC(ExpectedPRK));
  WriteLn('Actual PRK:   ', UBytesToHexLC(PRK));

  OKM := UHKDF_Expand_SHA2_256(PRK, Info, HashLength);
  WriteLn('Expected OKM: ', UBytesToHexLC(ExpectedOKM));
  WriteLn('Actual OKM:   ', UBytesToHexLC(OKM));
  PassedFailed(UBytesCompare(ExpectedOKM, OKM) = 0, TestName);
end;

procedure Test_RFC5869;
begin
  Test_SHA2_256(
    UHexToBytes('0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b'),
    UHexToBytes('000102030405060708090a0b0c'),
    UHexToBytes('f0f1f2f3f4f5f6f7f8f9'),
    UHexToBytes('077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5'),
    UHexToBytes('3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865'),
    42,
    'RFC5869 Basic test case with SHA-256'
  );
  Test_SHA2_256(
    UHexToBytes('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f'),
    UHexToBytes('606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf'),
    UHexToBytes('b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff'),
    UHexToBytes('06a6b88c5853361a06104c9ceb35b45cef760014904671014a193f40c15fc244'),
    UHexToBytes('b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87'),
    82,
    'RFC5869 SHA-256 and longer inputs/outputs'
  );
  Test_SHA2_256(
    UHexToBytes('0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b'),
    nil,
    nil,
    UHexToBytes('19ef24a32c717b167f33a91d6f648bdf96596776afdb6377ac434c1c293ccb04'),
    UHexToBytes('8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8'),
    42,
    'RFC5869 SHA-256 and zero-length salt/info'
  );
end;

begin
  Test_RFC5869;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

