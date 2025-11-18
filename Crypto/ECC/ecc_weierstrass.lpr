program ecc_weierstrass;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CryptoUtils,
  CommonUtils;

procedure TestRFC6979;
  var Curve: TUECC.Weierstrass.TCurve;
  var PrivateKey, MessageHash: TUECC.TBigInt;
  var Sig: TUECC.Weierstrass.TSignature;
begin
  Curve := TUECC.Weierstrass.TCurve.Make_SECP256R1;
  PrivateKey := '#c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721';
  MessageHash := TUECC.TBigInt.Make(USHA2_256('sample'));
  Sig := TUECC.Weierstrass.Sign(Curve, PrivateKey, MessageHash);
  WriteLn('r:          ', LowerCase(Sig.r.ToHex));
  WriteLn('Expected r: efd48b2aacb6a8fd1140dd9cd45e81d69d2c877b56aaf991c34d0ea84eaf3716');
  WriteLn('s:          ', LowerCase(Sig.s.ToHex));
  WriteLn('Expected s: f7cb1c942d657c41d436c7a1b6e29f65f3e900dbb9aff4064dc4ab2f843acda8');
end;

procedure TestECDSA;
  var Curve: TUECC.Weierstrass.TCurve;
  var Key: TUECC.Weierstrass.TKey;
  var MessageHash: TUInt8Array;
  var Sig: TUECC.Weierstrass.TSignature;
  var Verify: Boolean;
begin
  UThreadRandomize;
  Curve := TUECC.Weierstrass.Curve_SECP256R1;
  Key := TUECC.Weierstrass.MakeKey(Curve);
  WriteLn('Key d:   ', Key.d.ToHex);
  WriteLn('Key q.x: ', Key.q.x.ToHex);
  WriteLn('Key q.y: ', Key.q.y.ToHex);
  MessageHash := USHA2_256('Hello, World!');
  WriteLn('MshHash: ', UBytesToHex(MessageHash));
  Sig := TUECC.Weierstrass.Sign(Curve, Key.d, TUECC.TBigInt.Make(MessageHash));
  WriteLn('Sig r: ', Sig.r.ToHex);
  WriteLn('Sig s: ', Sig.s.ToHex);
  Verify := TUECC.Weierstrass.Verify(Curve, Key.q, MessageHash, Sig);
  if Verify then
  begin
    WriteLn('Verify SUCCESS');
  end
  else
  begin
    WriteLn('Verify FAILED');
  end;
end;

procedure TestECDH;
  var KeyA, KeyB: TUECC.Weierstrass.TKey;
  var SharedA, SharedB: TUECC.TBigInt;
begin
  UThreadRandomize;
  KeyA := UMakeECCKey;
  WriteLn('Key A d:   ', LowerCase(KeyA.d.ToHex));
  WriteLn('Key A q.x: ', LowerCase(KeyA.q.x.ToHex));
  WriteLn('Key A q.y: ', LowerCase(KeyA.q.y.ToHex));
  KeyB := UMakeECCKey;
  WriteLn('Key B d:   ', LowerCase(KeyB.d.ToHex));
  WriteLn('Key B q.x: ', LowerCase(KeyB.q.x.ToHex));
  WriteLn('Key B q.y: ', LowerCase(KeyB.q.y.ToHex));
  SharedA := USharedKey_ECDH(KeyB.q, KeyA.d);
  WriteLn('Shared A: ', LowerCase(SharedA.ToHex));
  SharedB := USharedKey_ECDH(KeyA.q, KeyB.d);
  WriteLn('Shared B: ', LowerCase(SharedB.ToHex));
  if SharedA = SharedB then
  begin
    WriteLn('Shared Key SUCCESS');
  end
  else
  begin
    WriteLn('Shared Key FAILED');
  end;
end;

begin
  TestECDH;
  TestECDSA;
end.

