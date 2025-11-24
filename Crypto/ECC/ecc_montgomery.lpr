program ecc_montgomery;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CryptoUtils,
  CommonUtils;

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

procedure TestCurve25519(
  const Input, BasePoint, Expected: TUECC.TBigInt;
  const TestName: String
);
  var Curve: TUECC.Montgomery.TCurve;
  var Output: TUECC.TBigInt;
begin
  Curve := TUECC.Montgomery.Curve_25519;
  WriteLn('Testing Curve25519 ', TestName, '...');
  Output := TUECC.Montgomery.X25519(Curve, Input, BasePoint);
  WriteLn('Output:   ', LowerCase(Output.ToHex));
  WriteLn('Expected: ', LowerCase(Expected.ToHex));
  PassedFailed(Output = Expected, 'Test' + ' ' + TestName);
end;

procedure TestCurve25519_RFC7748;
begin
  TestCurve25519(
    '$c49a44ba44226a50185afcc10a4c1462dd5e46824b15163b9d7c52f06be346a5',
    '$4c1cabd0a603a9103b35b326ec2466727c5fb124a4c19435db3030586768dbe6',
    '$5285a2775507b454f7711c4903cfec324f088df24dea948e90c6e99d3755dac3',
    'RFC 7748 Vector 1'
  );
  TestCurve25519(
    '$0dba18799e16a42cd401eae021641bc1f56a7d959126d25a3c67b4d1d4e9664b',
    '$13a415c749d54cfc3e3cc06f10e7db312cae38059d95b7f4d3116878120f21e5',
    '$5779ac7a64f7f8e652a19f79685a598bf873b8b45ce4ad7a7d90e87694decb95',
    'RFC 7748 Vector 2'
  );
end;

procedure TestCurve25519_IteratedDH;
  var Curve: TUECC.Montgomery.TCurve;
  var k, u, Tmp, Expected1, Expected1000: TUECC.TBigInt;
  var i: Int32;
begin
  Curve := TUECC.Montgomery.Curve_25519;
  WriteLn('Testing iterated DH (RFC 7748 Section 5.2)...');
  Expected1 := '$7930ae1103e8603c784b85b67bb897789f27b72b3e0b35a1bcd727627a8e2c42';
  Expected1000 := '$512c53994db92e5f87e36093c487381c3c4d2f6f56ef0028550933a89bf54c68';
  k := 9;
  u := 9;
  for i := 1 to 1000 do
  begin
    Tmp := TUECC.Montgomery.X25519(Curve, k, u);
    u := k;
    k := Tmp;
    if i = 1 then
    begin
      WriteLn('After 1:  ', LowerCase(k.ToHex));
      WriteLn('Expected: ', LowerCase(Expected1.ToHex));
      PassedFailed(k = Expected1, 'Iterated test 1');
    end
    else if i = 1000 then
    begin
      WriteLn('After 1000: ', LowerCase(k.ToHex));
      WriteLn('Expected:   ', LowerCase(Expected1000.ToHex));
      PassedFailed(k = Expected1000, 'Iterated test 1');
    end;
  end;
end;

procedure TestCurve25519_ECDH;
  var Curve: TUECC.Montgomery.TCurve;
  var KeyA, KeyB: TUECC.Montgomery.TKey;
  var ExpectedShared, SharedA, SharedB: TUECC.TBigInt;
begin
  WriteLn('Testing ECDH (RFC 7748 Section 6.1)...');
  Curve := TUECC.Montgomery.Curve_25519;
  KeyA.d := '$2a2cb91da5fb77b12a99c0eb872f4cdf4566b25172c1163c7da518730a6d0777';
  KeyA.q := TUECC.Montgomery.DerivePublicKey(Curve, KeyA.d);
  KeyB.d := '$ebe088ff278b2f1cfdb6182629b13b6fe60e80838b7fe1794b8a4a627e08ab5d';
  KeyB.q := TUECC.Montgomery.DerivePublicKey(Curve, KeyB.d);
  ExpectedShared := '$4217161e3c9bf076339ed147c9217ee0250f3580f43b8e72e12dcea45b9d5d4a';
  SharedA := TUECC.Montgomery.SharedKey(Curve, KeyB.q, KeyA.d);
  SharedB := TUECC.Montgomery.SharedKey(Curve, KeyA.q, KeyB.d);
  WriteLn('PrivateA:  ', KeyA.d.ToHexLC);
  WriteLn('PublicA:   ', KeyA.q.ToHexLC);
  WriteLn('PrivateB:  ', KeyB.d.ToHexLC);
  WriteLn('PublicB:   ', KeyB.q.ToHexLC);
  WriteLn('SharedA:   ', SharedA.ToHexLC);
  WriteLn('SharedB:   ', SharedB.ToHexLC);
  WriteLn('Expected:  ', ExpectedShared.ToHexLC);
  PassedFailed((SharedA = SharedB) and (SharedA = ExpectedShared), 'ECDH');
end;

begin
  //HexSwap;
  TestCurve25519_RFC7748;
  TestCurve25519_IteratedDH;
  TestCurve25519_ECDH;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

