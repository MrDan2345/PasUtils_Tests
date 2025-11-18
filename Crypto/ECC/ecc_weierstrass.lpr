program ecc_weierstrass;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CryptoUtils,
  CommonUtils;

procedure TestECDSA;
  var Curve: TUECC.Weierstrass.TCurve;
  var Key: TUECC.Weierstrass.TKey;
  var MessageHash: TUInt8Array;
  var Sig: TUECC.Weierstrass.TSignature;
  var Verify: Boolean;
begin
  WriteLn('Testing ECDSA...');
  UThreadRandomize;
  Curve := TUECC.Weierstrass.Curve_SECP256R1;
  Key := TUECC.Weierstrass.MakeKey(Curve);
  WriteLn('Key d:   ', Key.d.ToHexLC);
  WriteLn('Key q.x: ', Key.q.x.ToHexLC);
  WriteLn('Key q.y: ', Key.q.y.ToHexLC);
  MessageHash := USHA2_256('Hello, World!');
  WriteLn('MsgHash: ', LowerCase(UBytesToHex(MessageHash)));
  Sig := TUECC.Weierstrass.Sign(Curve, Key.d, TUECC.TBigInt.Make(MessageHash));
  WriteLn('Sig r: ', Sig.r.ToHexLC);
  WriteLn('Sig s: ', Sig.s.ToHexLC);
  Verify := TUECC.Weierstrass.Verify(Curve, Key.q, MessageHash, Sig);
  if Verify then
  begin
    WriteLn('✅ ECDSA PASSED!');
  end
  else
  begin
    WriteLn('❌ ECDSA FAILED!');
  end;
end;

procedure TestECDH;
  var KeyA, KeyB: TUECC.Weierstrass.TKey;
  var SharedA, SharedB: TUECC.TBigInt;
begin
  WriteLn('Testing ECDH...');
  UThreadRandomize;
  KeyA := UMakeECCKey;
  WriteLn('Key A d:   ', KeyA.d.ToHexLC);
  WriteLn('Key A q.x: ', KeyA.q.x.ToHexLC);
  WriteLn('Key A q.y: ', KeyA.q.y.ToHexLC);
  KeyB := UMakeECCKey;
  WriteLn('Key B d:   ', KeyB.d.ToHexLC);
  WriteLn('Key B q.x: ', KeyB.q.x.ToHexLC);
  WriteLn('Key B q.y: ', KeyB.q.y.ToHexLC);
  SharedA := USharedKey_ECDH(KeyB.q, KeyA.d);
  WriteLn('Shared A: ', SharedA.ToHexLC);
  SharedB := USharedKey_ECDH(KeyA.q, KeyB.d);
  WriteLn('Shared B: ', SharedB.ToHexLC);
  if SharedA = SharedB then
  begin
    WriteLn('✅ ECDH PASSED!');
  end
  else
  begin
    WriteLn('❌ ECDH FAILED!');
  end;
end;

procedure TestKeyGeneration;
  var Curve: TUECC.Weierstrass.TCurve;
  var Key: TUECC.Weierstrass.TKey;
  var Expected: TUECC.Weierstrass.TPoint;
begin
  WriteLn('Testing Key Generation...');
  Expected.x := '$60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6';
  Expected.y := '$7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299';
  Curve := TUECC.Weierstrass.Curve_SECP256R1;
  Key.d := '$c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721';
  Key.q := TUECC.Weierstrass.DerivePublicKey(Curve, Key.d);
  WriteLn('Private:  ', Key.d.ToHexLC);
  WriteLn('Public X: ', Key.q.x.ToHexLC);
  WriteLn('Public Y: ', Key.q.y.ToHexLC);
  if (Expected.x = Key.q.x) and (Expected.y = Key.q.y) then
  begin
    WriteLn('✅ Key Generation PASSED!');
  end
  else
  begin
    WriteLn('❌ Key Generation FAILED!');
  end;
end;

begin
  TestKeyGeneration;
  TestECDH;
  TestECDSA;
end.

