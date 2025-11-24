program ecc_edwards;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CryptoUtils,
  CommonUtils;

var Curve: TUECC.Edwards.TCurve;

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

procedure Test_ScalarMultiply;
  var Scalar: TUECC.TBigInt;
  var Result, Expected: TUECC.Edwards.TPoint;
begin
  WriteLn('Testing ScalarMultiply...');
  // Test 1: 1 * G = G
  Scalar := TUECC.TBigInt.One;
  Result := TUECC.Edwards.ScalarMultiply(Curve, Scalar, Curve.b);
  Result := Curve.ToAffine(Result);
  Expected := Curve.ToAffine(Curve.b);

  WriteLn('Test 1: 1 * G = G');
  WriteLn('Result.x:   ', Result.x.ToHex);
  WriteLn('Expected.x: ', Expected.x.ToHex);
  WriteLn('Result.y:   ', Result.y.ToHex);
  WriteLn('Expected.y: ', Expected.y.ToHex);

  PassedFailed((Result.x = Expected.x) and (Result.y = Expected.y), 'Test 1: 1 * G = G');

  // Test 2: 2 * G
  Scalar := 2;
  Result := TUECC.Edwards.ScalarMultiply(Curve, Scalar, Curve.b);
  Result := Curve.ToAffine(Result);

  // Manually compute 2 * G = G + G
  Expected := TUECC.Edwards.PointDouble(Curve, Curve.b);
  Expected := Curve.ToAffine(Expected);

  WriteLn('Test 2: 2 * G');
  WriteLn('Result.x:   ', Result.x.ToHex);
  WriteLn('Expected.x: ', Expected.x.ToHex);

  PassedFailed((Result.x = Expected.x) and (Result.y = Expected.y), 'Test 2: 2 * G = G + G');

  // Test 3: n * G = neutral
  Result := TUECC.Edwards.ScalarMultiply(Curve, Curve.n, Curve.b);

  WriteLn('Test 3: n * G = neutral');
  PassedFailed(Result.IsNeutral, 'Test 3: n * G = neutral');
end;

procedure Test_PointCompression;
  var Original, Decompressed, AffineOrig, AffineDecomp: TUECC.Edwards.TPoint;
  var Compressed: TUECC.Edwards.TPointCompressed;
begin
  WriteLn('=== Testing Point Compression ===');

  // Test with base point
  //Original := Curve.b;
  Original := TUECC.Edwards.ScalarMultiply(Curve, Random(1000000), Curve.b);

  WriteLn('Original point (extended coords):');
  WriteLn('X: ', Original.x.ToHex);
  WriteLn('Y: ', Original.y.ToHex);
  WriteLn('Z: ', Original.z.ToHex);
  WriteLn('T: ', Original.t.ToHex);

  AffineOrig := Curve.ToAffine(Original);
  WriteLn('Original point (affine):');
  WriteLn('x: ', AffineOrig.x.ToHex);
  WriteLn('y: ', AffineOrig.y.ToHex);

  // Compress
  Compressed := TUECC.Edwards.PointCompress(Curve, Original);
  WriteLn('Compressed: ', UBytesToHexLC(Compressed));

  // Decompress
  Decompressed := TUECC.Edwards.PointDecompress(Curve, Compressed);

  if not Decompressed.IsValid then
  begin
    WriteLn('❌ Decompression failed - invalid point!');
    Exit;
  end;

  WriteLn('Decompressed point (extended coords):');
  WriteLn('X: ', Decompressed.x.ToHex);
  WriteLn('Y: ', Decompressed.y.ToHex);
  WriteLn('Z: ', Decompressed.z.ToHex);
  WriteLn('T: ', Decompressed.t.ToHex);

  AffineDecomp := Curve.ToAffine(Decompressed);
  WriteLn('Decompressed point (affine):');
  WriteLn('x: ', AffineDecomp.x.ToHex);
  WriteLn('y: ', AffineDecomp.y.ToHex);

  // Compare
  PassedFailed(AffineOrig = AffineDecomp, 'Compression/Decompression round-trip');
end;

procedure Test_BasePointDecompression;
  var PointCompr: TUECC.Edwards.TPointCompressed;
  var PointDecompr: TUECC.Edwards.TPoint;
  var a, b: TUECC.Edwards.TPoint;
begin
  WriteLn('Testing point decompression...');
  PointCompr := TUECC.Edwards.PointCompress(Curve, Curve.b);
  PointDecompr := TUECC.Edwards.PointDecompress(Curve, PointCompr);
  a := Curve.ToAffine(Curve.b);
  b := Curve.ToAffine(PointDecompr);
  PassedFailed((a.x = b.x) and (a.y = b.y), 'Point Decompression');
end;

procedure Test_SigningVerification;
  var Key: TUECC.Edwards.TKey;
  var Message: TUInt8Array;
  var Signature: TUECC.Edwards.TSignature;
  var Valid: Boolean;
begin
  WriteLn('Testing Signing and Verification...');
  key := TUECC.Edwards.MakeKey(Curve);
  WriteLn('Private key: ', UBytesToHexLC(Key.d));
  WriteLn('Public key:  ', UBytesToHexLC(Key.q));
  Message := 'Hello, Ed25519!';
  Signature := TUECC.Edwards.Sign_Ed25519(Curve, Key, Message);
  WriteLn('Signature R: ', UBytesToHexLC(Signature.r));
  WriteLn('Signature S: ', Signature.s.ToHexLC);
  Valid := TUECC.Edwards.Verify_Ed25519(Curve, Key.q, Message, Signature);
  PassedFailed(Valid, 'Signing and Verification');
end;

procedure Test_RFC8032;
  var Key: TUECC.Edwards.TKey;
  var KeyPrivate: TUInt8Array;
  var Sig: TUECC.Edwards.TSignature;
  var SigR, SigS: String;
  const ExpectedR = 'e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e06522490155';
  const ExpectedS = '5fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b';
  const ExpectedPublic = 'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a';
begin
  KeyPrivate := UHexToBytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60');
  Key := TUECC.Edwards.MakeKey(Curve, KeyPrivate);
  Sig := TUECC.Edwards.Sign_Ed25519(Curve, Key, nil);
  SigR := UBytesToHexLC(Sig.r);
  SigS := UBytesToHexLC(Sig.s.ToBytes);
  WriteLn('Private:  ', UBytesToHexLC(Key.d));
  WriteLn('ExpectedPublic: ', ExpectedPublic);
  WriteLn('ActualPublic:   ', UBytesToHexLC(Key.q));
  WriteLn('ExpectedR: ', ExpectedR);
  WriteLn('ActualR:   ', SigR);
  WriteLn('ExpectedS: ', ExpectedS);
  WriteLn('ActualS:   ', SigS);
  PassedFailed((ExpectedR = SigR) and (ExpectedS = SigS), 'RFC8032');
end;

begin
  Randomize;
  Curve := TUECC.Edwards.Curve_Ed25519;
  Test_BasePointDecompression;
  Test_ScalarMultiply;
  Test_PointCompression;
  Test_RFC8032;
  Test_SigningVerification;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

