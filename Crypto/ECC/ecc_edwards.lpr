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

procedure Test_BasePoint;
  var Scalar: TUECC.TBigInt;
  var BasePoint: TUECC.Edwards.TPoint;
begin
  WriteLn('Testing base point...');
  Scalar := TUECC.TBigInt.One;
  BasePoint := TUECC.Edwards.ScalarMultiply(Curve, Scalar, Curve.b);
  PassedFailed(BasePoint = Curve.b, 'Base Point Test');
end;

procedure Test_BasePointCompression;
  var PointCompr: TUECC.Edwards.TPointCompressed;
  const ExpectedCompr: String = '5866666666666666666666666666666666666666666666666666666666666666';
begin
  WriteLn('Testing base point compression...');
  PointCompr := TUECC.Edwards.PointCompress(Curve, Curve.b);
  WriteLn('Compressed: ', UBytesToHex(PointCompr));
  PassedFailed(UBytesToHex(PointCompr) = ExpectedCompr, 'Base Point Compression');
end;

procedure Test_PointDecompression;
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
  var d, q: TUECC.TBigInt;
  var Sig: TUECC.Edwards.TSignature;
  var SigStr: String;
  const ExpectedSignature = '0b107a8e4341516524be5b59f0f55bd26bb4f91c70391ec6ac3ba3901582b85f5501492265e073d874d9e5b81e7f87848a826e80cce2869072ac60c3004356e5';
begin
  d := '$607fae1c03ac3b701969327b69c54944c42cec92f44a84ba605afdef9db1619d';
  UInit(Key.d, d, SizeOf(Key.d));
  q := '$1a5107f7681a02af2523a6daf372e10e3a0764c9d3fe4bd5b70ab18201985ad7';
  UInit(Key.q, q, SizeOf(Key.q));
  Sig := TUECC.Edwards.Sign_Ed25519(Curve, Key, nil);
  SigStr := Sig.ToHex;
  WriteLn('Private:  ', d.ToHexLC);
  WriteLn('Public:   ', q.ToHexLC);
  WriteLn('Expected: ', ExpectedSignature);
  WriteLn('Actual:   ', SigStr);
  PassedFailed(ExpectedSignature = SigStr, 'RFC8032');
end;

begin
  Curve := TUECC.Edwards.Curve_Ed25519;
  //Test_BasePoint;
  //Test_BasePointCompression;
  //Test_PointDecompression;
  //Test_SigningVerification;
  Test_RFC8032;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

