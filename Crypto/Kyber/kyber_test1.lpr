program kyber_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  Math,
  CommonUtils,
  CryptoUtils;

class function TestNTT: Boolean;
var
  Original, Transformed, Recovered: TUKyber768.TPoly;
  i: Int32;
  AllMatch: Boolean;
begin
  WriteLn('Testing NTT/InvNTT...');
  for i := 0 to TUKyber768.N - 1 do
  begin
    Original[i] := i mod 17;
  end;
  Transformed := Original;
  TUKyber768.PolyNTT(Transformed);

  WriteLn('After NTT, first 8 coefficients:');
  for i := 0 to 7 do
  begin
    Write(Transformed[i]:6, ' ');
  end;
  WriteLn;

  Recovered := Transformed;
  TUKyber768.PolyInvNTT(Recovered);
  TUKyber768.FromMont(Recovered);

  WriteLn('After InvNTT, first 8 coefficients:');
  for i := 0 to 7 do
  begin
    Write(Recovered[i]:6, ' ');
  end;
  WriteLn;

  WriteLn('Original, first 8 coefficients:');
  for i := 0 to 7 do
  begin
    Write(Original[i]:6, ' ');
  end;
  WriteLn;

  AllMatch := True;
  for i := 0 to TUKyber768.N - 1 do
  begin
    if Recovered[i] = Original[i] then Continue;
    if AllMatch then WriteLn('Mismatches found:');
    WriteLn(
      '  Index ', i:3, ': original=', Original[i]:5,
      ' recovered=', Recovered[i]:5
    );
    AllMatch := False;
    if i > 10 then Break;
  end;

  Result := AllMatch;

  if Result then
  begin
    WriteLn('✅ NTT/InvNTT test PASSED!')
  end
  else
  begin
    WriteLn('❌ NTT/InvNTT test FAILED!');
  end;
end;

class function TestCompression: Boolean;
  var original, recovered: TUKyber768.TPoly;
  var compressed: TUInt8Array;
  var i: Int32;
  var d: Int32;
  var MaxError: Int32;
begin
  WriteLn('Testing compression...');

  {for i := 0 to TUKyber768.N - 1 do
  begin
    original[i] := (i * 7) mod TUKyber768.Q;
  end;}
  original := TUKyber768.GenerateKeyPair.PublicKey.t[0];

  compressed := TUKyber768.PolyCompress(original);
  WriteLn('Compressed to ', Length(compressed), ' bytes');

  recovered := TUKyber768.PolyDecompress(compressed);

  MaxError := TUKyber768.Q div (2 ** (4+1));
  WriteLn('MaxError = ', MaxError);
  Result := True;
  for i := 0 to TUKyber768.N - 1 do
  begin
    d := TUKyber768.DistModQ(recovered[i], original[i]);
    //d := Abs(recovered[i] - original[i]) mod TUKyber768.Q;
    //d := UMin(d, TUKyber768.Q - d);
    WriteLn('[', i, '] ', original[i], ' - ', recovered[i], ' = ', d);
    if d > MaxError then
    begin
      Result := False;
      WriteLn('Large error at ', i, ' = ', d);
    end;
  end;

  if Result then
  begin
    WriteLn('✅ Compression test PASSED!')
  end
  else
  begin
    WriteLn('❌ Compression test FAILED!');
  end;
end;

class function TestCompression2: Boolean;
  var original, recovered: TUKyber768.TVec;
  var compressed: TUInt8Array;
  var i, j: Int32;
  var d: Int32;
  var MaxError: Int32;
begin
  WriteLn('Testing vector compression...');

  original := TUKyber768.GenerateKeyPair.PublicKey.t;

  compressed := TUKyber768.VecCompress(original);
  WriteLn('Compressed to ', Length(compressed), ' bytes');

  recovered := TUKyber768.VecDecompress(compressed);

  MaxError := TUKyber768.Q div (2 ** (4+1));
  WriteLn('MaxError = ', MaxError);
  Result := True;
  for i := 0 to TUKyber768.K - 1 do
  for j := 0 to TUKyber768.N - 1 do
  begin
    d := Abs(recovered[i][j] - original[i][j]);
    WriteLn('[', i, ',', j,'] ', original[i][j], ' - ', recovered[i][j], ' = ', d);
    if d > MaxError then
    begin
      Result := False;
      WriteLn('Large error at ', i, ' = ', d);
    end;
  end;

  if Result then
  begin
    WriteLn('✅ Compression test PASSED!')
  end
  else
  begin
    WriteLn('❌ Compression test FAILED!');
  end;
end;

class function TestSerialization: Boolean;
  var Original, Recovered: TUKyber768.TVec;
  var Serialized: TUInt8Array;
  var i, j: Int32;
begin
  WriteLn('Testing compression...');

  for i := 0 to TUKyber768.K - 1 do
  for j := 0 to TUKyber768.N - 1 do
  begin
    Original[i][j] := (j * 7 + i) mod TUKyber768.Q;
  end;

  Serialized := TUKyber768.VecToBytes(Original);
  WriteLn('Compressed to ', Length(Serialized), ' bytes');

  Recovered := TUKyber768.VecFromBytes(Serialized);

  Result := True;
  for i := 0 to TUKyber768.K - 1 do
  for j := 0 to TUKyber768.N - 1 do
  begin
    if Recovered[i][j] <> Original[i][j] then
    begin
      Result := False;
      WriteLn('Mismatch [', i, ', ', j, ']: original = ', Original[i][j], ', recovered = ', Recovered[i][j]);
    end;
    WriteLn('[', i, ', ', j, '] = ', Original[i][j]);
  end;

  if Result then
  begin
    WriteLn('✅ Serialization test PASSED!')
  end
  else
  begin
    WriteLn('❌ Serialization test FAILED!');
  end;
end;

function TestKeyGeneration: Boolean;
  function CoeffsEqualModQ(a, b: Int32): Boolean;
    var diff: Int32;
  begin
    diff := (a - b) mod TUKyber768.Q;
    if diff < 0 then diff += TUKyber768.Q;
    Result := diff = 0;
  end;
  var KeyPair: TUKyber768.TKeyPair;
  var pk_bytes, sk_bytes: TUInt8Array;
  var pk_recovered: TUKyber768.TPublicKey;
  var sk_recovered: TUKyber768.TSecretKey;
  var i, j: Int32;
begin
  WriteLn('Testing key generation...');

  // Generate a KeyPair
  KeyPair := TUKyber768.GenerateKeyPair;

  WriteLn('Public key t[0][0..7]: ');
  for i := 0 to 7 do
  begin
    Write(KeyPair.PublicKey.t[0][i]:6, ' ');
  end;
  WriteLn;

  WriteLn('Secret key s[0][0..7]: ');
  for i := 0 to 7 do
  begin
    Write(KeyPair.SecretKey.s[0][i]:6, ' ');
  end;
  WriteLn;

  // Test serialization
  pk_bytes := TUKyber768.PackPublicKey(KeyPair.PublicKey);
  sk_bytes := TUKyber768.PackSecretKey(KeyPair.SecretKey);

  WriteLn('Public key size: ', Length(pk_bytes), ' bytes');
  WriteLn('Secret key size: ', Length(sk_bytes), ' bytes');

  // Test deserialization
  pk_recovered := TUKyber768.UnpackPublicKey(pk_bytes);
  sk_recovered := TUKyber768.UnpackSecretKey(sk_bytes);

  // Verify they match
  Result := True;
  for i := 0 to TUKyber768.K - 1 do
  begin
    for j := 0 to TUKyber768.N - 1 do
    begin
      if pk_recovered.t[i][j] <> KeyPair.PublicKey.t[i][j] then
      begin
        WriteLn('PK mismatch at t[', i, '][', j, ']');
        Result := False;
      end;
      WriteLn('[', i, ', ', j, '] = ', KeyPair.SecretKey.s[i][j]);
      if not CoeffsEqualModQ(sk_recovered.s[i][j], KeyPair.SecretKey.s[i][j]) then
      begin
        WriteLn('SK mismatch at s[', i, '][', j, '] = ', sk_recovered.s[i][j]);
        Result := False;
      end;
    end;
  end;

  if Result then
  begin
    WriteLn('✅ Key generation test PASSED!')
  end
  else
  begin
    WriteLn('❌ Key generation test FAILED!');
  end;
end;

function TestKeyGeneration2: Boolean;
  var KeyPair: TUKyber768.TKeyPair;
  var pk_bytes, sk_bytes: TUInt8Array;
  var pk_recovered: TUKyber768.TPublicKey;
  var sk_recovered: TUKyber768.TSecretKey;
  var i, j: Int32;
begin
  WriteLn('Testing key generation...');

  KeyPair := TUKyber768.GenerateKeyPair;
  WriteLn('Secret key:');
  for i := 0 to High(KeyPair.SecretKey.s) do
  begin
    for j := 0 to High(KeyPair.SecretKey.s[i]) do
    begin
      if j > 0 then Write(',');
      Write(KeyPair.SecretKey.s[i][j]);
    end;
    WriteLn;
    WriteLn;
  end;

  WriteLn('Public key:');
  WriteLn('Rho:');
  for i := 0 to High(KeyPair.PublicKey.Rho) do
  begin
    if i > 0 then Write(',');
    Write(KeyPair.PublicKey.Rho[i]);
  end;
  WriteLn;
  WriteLn;
  WriteLn('t:');
  for i := 0 to High(KeyPair.PublicKey.t) do
  begin
    for j := 0 to High(KeyPair.SecretKey.s[i]) do
    begin
      if j > 0 then Write(',');
      Write(KeyPair.PublicKey.t[i][j]);
    end;
    WriteLn;
    WriteLn;
  end;

  Result := True;
  if Result then
  begin
    WriteLn('✅ Key generation test PASSED!')
  end
  else
  begin
    WriteLn('❌ Key generation test FAILED!');
  end;
end;

function TestMessageEncoding: Boolean;
  var msg, recovered: TUInt8Array;
  var poly: TUKyber768.TPoly;
  var i: Int32;
begin
  msg := TUInt8Array.Make(TUKyber768.SYMBYTES);
  for i := 0 to TUKyber768.SYMBYTES - 1 do
  begin
    msg[i] := Random(256);
  end;
  poly := TUKyber768.PolyFromMsg(msg);
  recovered := TUKyber768.PolyToMsg(poly);
  Result := True;
  for i := 0 to TUKyber768.SYMBYTES - 1 do
  if msg[i] <> recovered[i] then
  begin
    WriteLn('Mismatch at byte ', i, ': ', msg[i], ' vs ', recovered[i]);
    Result := False;
  end;
  if Result then
  begin
    WriteLn('✓ Message encoding test PASSED!');
  end
  else
  begin
    WriteLn('✗ Message encoding test FAILED!');
  end;
end;

function TestPolyMulCorrectness: Boolean;
var
  a, b, expected, viaNTT, a_ntt, b_ntt: TUKyber768.TPoly;
  i: Int32;
begin
  for i := 0 to TUKyber768.N - 1 do
  begin
    a[i] := Random(TUKyber768.Q);
    b[i] := Random(TUKyber768.Q);
  end;

  expected := TUKyber768.PolyMulNaive(a, b);   // trusted O(n^2) reference

  a_ntt := a; TUKyber768.PolyNTT(a_ntt);
  b_ntt := b; TUKyber768.PolyNTT(b_ntt);
  viaNTT := TUKyber768.PolyMul(a_ntt, b_ntt);  // pointwise/basemul in NTT domain
  TUKyber768.PolyInvNTT(viaNTT);

  Result := True;
  for i := 0 to TUKyber768.N - 1 do
    if not TUKyber768.CoeffsEqualModQ(viaNTT[i], expected[i]) then
    begin
      WriteLn('Mismatch at ', i, ': naive=', expected[i], ' ntt=', viaNTT[i]);
      Result := False;
    end;

  if Result then WriteLn('✓ PolyMul (NTT) matches PolyMulNaive')
  else WriteLn('✗ PolyMul (NTT) does NOT match PolyMulNaive');
end;

function TestEncryption: Boolean;
  var KeyPair: TUKyber768.TKeyPair;
  var Msg, Coins, Recovered, Ciphertext: TUInt8Array;
  var i: Int32;
begin
  WriteLn('Testing encrypt/decrypt...');

  KeyPair := TUKyber768.GenerateKeyPair;

  Msg := TUInt8Array.Make(TUKyber768.SYMBYTES);
  for i := 0 to TUKyber768.SYMBYTES - 1 do
  begin
    Msg[i] := Random(256);
  end;

  Coins := TUInt8Array.Make(TUKyber768.SYMBYTES);
  for i := 0 to TUKyber768.SYMBYTES - 1 do
  begin
    Coins[i] := Random(256);
  end;

  Ciphertext := TUKyber768.Encrypt(KeyPair.PublicKey, Msg, Coins);
  WriteLn('Ciphertext size: ', Length(Ciphertext), ' bytes');

  Recovered := TUKyber768.Decrypt(KeyPair.SecretKey, Ciphertext);

  Result := True;
  for i := 0 to TUKyber768.SYMBYTES - 1 do
  begin
    if Msg[i] <> Recovered[i] then
    begin
      WriteLn('Mismatch at byte ', i, ': sent=', Msg[i], ' recovered=', Recovered[i]);
      Result := False;
    end;
  end;

  if Result then
  begin
    WriteLn('✅ Encrypt/Decrypt test PASSED!')
  end
  else
  begin
    WriteLn('❌ Encrypt/Decrypt test FAILED!');
  end;
end;

procedure Test;
begin
end;

procedure Run;
begin
  //Test;
  //{
  //TestNTT;
  //WriteLn;
  //TestCompression;
  //WriteLn;
  //TestSerialization;
  //WriteLn;
  //TestKeyGeneration;
  //TestMessageEncoding;
  //TestPolyMulCorrectness;
  TestEncryption;
  //}
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

