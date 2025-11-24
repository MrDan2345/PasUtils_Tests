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
  procedure Test(
    const Private: TUInt8Array;
    const ExpectedPublic: TUInt8Array;
    const ExpectedR: TUInt8Array;
    const ExpectedS: TUInt8Array;
    const Message: TUInt8Array;
    const Caption: String
  );
    var Key: TUECC.Edwards.TKey;
    var Sig: TUECC.Edwards.TSignature;
    var SigR, SigS: String;
  begin
    Key := TUECC.Edwards.MakeKey(Curve, Private);
    Sig := TUECC.Edwards.Sign_Ed25519(Curve, Key, Message);
    SigR := UBytesToHexLC(Sig.r);
    SigS := UBytesToHexLC(Sig.s.ToBytes);
    WriteLn('Private:  ', UBytesToHexLC(Key.d));
    WriteLn('ExpectedPublic: ', UBytesToHexLC(ExpectedPublic));
    WriteLn('ActualPublic:   ', UBytesToHexLC(Key.q));
    WriteLn('ExpectedR: ', UBytesToHexLC(ExpectedR));
    WriteLn('ActualR:   ', SigR);
    WriteLn('ExpectedS: ', UBytesToHexLC(ExpectedS));
    WriteLn('ActualS:   ', SigS);
    PassedFailed(
      (UBytesToHexLC(ExpectedR) = SigR) and (UBytesToHexLC(ExpectedS) = SigS),
      'RFC8032' + ' ' + Caption
    );
  end;
  var Message1024: String;
begin
  WriteLn('Testing RFC8032 Vectors');
  Test(
    UHexToBytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60'), // Private
    UHexToBytes('d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a'), // Expected Public
    UHexToBytes('e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e06522490155'), // Expected R
    UHexToBytes('5fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b'), // Expected S
    nil,
    'Vector 1'
  );
  Test(
    UHexToBytes('4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb'), // Private
    UHexToBytes('3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c'), // Expected Public
    UHexToBytes('92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da'), // Expected R
    UHexToBytes('085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c'), // Expected S
    UHexToBytes('72'),
    'Vector 2'
  );
  Test(
    UHexToBytes('c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7'), // Private
    UHexToBytes('fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025'), // Expected Public
    UHexToBytes('6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac'), // Expected R
    UHexToBytes('18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a'), // Expected S
    UHexToBytes('af82'),
    'Vector 3'
  );
  Message1024 :=
    '08b8b2b733424243760fe426a4b54908' +
    '632110a66c2f6591eabd3345e3e4eb98' +
    'fa6e264bf09efe12ee50f8f54e9f77b1' +
    'e355f6c50544e23fb1433ddf73be84d8' +
    '79de7c0046dc4996d9e773f4bc9efe57' +
    '38829adb26c81b37c93a1b270b20329d' +
    '658675fc6ea534e0810a4432826bf58c' +
    '941efb65d57a338bbd2e26640f89ffbc' +
    '1a858efcb8550ee3a5e1998bd177e93a' +
    '7363c344fe6b199ee5d02e82d522c4fe' +
    'ba15452f80288a821a579116ec6dad2b' +
    '3b310da903401aa62100ab5d1a36553e' +
    '06203b33890cc9b832f79ef80560ccb9' +
    'a39ce767967ed628c6ad573cb116dbef' +
    'efd75499da96bd68a8a97b928a8bbc10' +
    '3b6621fcde2beca1231d206be6cd9ec7' +
    'aff6f6c94fcd7204ed3455c68c83f4a4' +
    '1da4af2b74ef5c53f1d8ac70bdcb7ed1' +
    '85ce81bd84359d44254d95629e9855a9' +
    '4a7c1958d1f8ada5d0532ed8a5aa3fb2' +
    'd17ba70eb6248e594e1a2297acbbb39d' +
    '502f1a8c6eb6f1ce22b3de1a1f40cc24' +
    '554119a831a9aad6079cad88425de6bd' +
    'e1a9187ebb6092cf67bf2b13fd65f270' +
    '88d78b7e883c8759d2c4f5c65adb7553' +
    '878ad575f9fad878e80a0c9ba63bcbcc' +
    '2732e69485bbc9c90bfbd62481d9089b' +
    'eccf80cfe2df16a2cf65bd92dd597b07' +
    '07e0917af48bbb75fed413d238f5555a' +
    '7a569d80c3414a8d0859dc65a46128ba' +
    'b27af87a71314f318c782b23ebfe808b' +
    '82b0ce26401d2e22f04d83d1255dc51a' +
    'ddd3b75a2b1ae0784504df543af8969b' +
    'e3ea7082ff7fc9888c144da2af58429e' +
    'c96031dbcad3dad9af0dcbaaaf268cb8' +
    'fcffead94f3c7ca495e056a9b47acdb7' +
    '51fb73e666c6c655ade8297297d07ad1' +
    'ba5e43f1bca32301651339e22904cc8c' +
    '42f58c30c04aafdb038dda0847dd988d' +
    'cda6f3bfd15c4b4c4525004aa06eeff8' +
    'ca61783aacec57fb3d1f92b0fe2fd1a8' +
    '5f6724517b65e614ad6808d6f6ee34df' +
    'f7310fdc82aebfd904b01e1dc54b2927' +
    '094b2db68d6f903b68401adebf5a7e08' +
    'd78ff4ef5d63653a65040cf9bfd4aca7' +
    '984a74d37145986780fc0b16ac451649' +
    'de6188a7dbdf191f64b5fc5e2ab47b57' +
    'f7f7276cd419c17a3ca8e1b939ae49e4' +
    '88acba6b965610b5480109c8b17b80e1' +
    'b7b750dfc7598d5d5011fd2dcc5600a3' +
    '2ef5b52a1ecc820e308aa342721aac09' +
    '43bf6686b64b2579376504ccc493d97e' +
    '6aed3fb0f9cd71a43dd497f01f17c0e2' +
    'cb3797aa2a2f256656168e6c496afc5f' +
    'b93246f6b1116398a346f1a641f3b041' +
    'e989f7914f90cc2c7fff357876e506b5' +
    '0d334ba77c225bc307ba537152f3f161' +
    '0e4eafe595f6d9d90d11faa933a15ef1' +
    '369546868a7f3a45a96768d40fd9d034' +
    '12c091c6315cf4fde7cb68606937380d' +
    'b2eaaa707b4c4185c32eddcdd306705e' +
    '4dc1ffc872eeee475a64dfac86aba41c' +
    '0618983f8741c5ef68d3a101e8a3b8ca' +
    'c60c905c15fc910840b94c00a0b9d0'
  ;
  Test(
    UHexToBytes('f5e5767cf153319517630f226876b86c8160cc583bc013744c6bf255f5cc0ee5'), // Private
    UHexToBytes('278117fc144c72340f67d0f2316e8386ceffbf2b2428c9c51fef7c597f1d426e'), // Expected Public
    UHexToBytes('0aab4c900501b3e24d7cdf4663326a3a87df5e4843b2cbdb67cbf6e460fec350'), // Expected R
    UHexToBytes('aa5371b1508f9f4528ecea23c436d94b5e8fcd4f681e30a6ac00a9704a188a03'), // Expected S
    UHexToBytes(Message1024),
    'Vector 1024'
  );
  Test(
    UHexToBytes('833fe62409237b9d62ec77587520911e9a759cec1d19755b7da901b96dca3d42'), // Private
    UHexToBytes('ec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf'), // Expected Public
    UHexToBytes('dc2a4459e7369633a52b1bf277839a00201009a3efbf3ecb69bea2186c26b589'), // Expected R
    UHexToBytes('09351fc9ac90b3ecfdfbc7c66431e0303dca179c138ac17ad9bef1177331a704'), // Expected S
    UHexToBytes('ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f'),
    'SHA(abc)'
  );
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

