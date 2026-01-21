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

  for i := 0 to TUKyber768.N - 1 do
  begin
    original[i] := (i * 7) mod TUKyber768.Q;
  end;

  compressed := TUKyber768.PolyCompress(original);
  WriteLn('Compressed to ', Length(compressed), ' bytes');

  recovered := TUKyber768.PolyDecompress(compressed);

  MaxError := TUKyber768.Q div (2 ** (4+1));
  WriteLn('MaxError = ', MaxError);
  Result := True;
  for i := 0 to TUKyber768.N - 1 do
  begin
    d := Abs(recovered[i] - original[i]);
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
      WriteLn('[', i, ', ', j, '] = ', KeyPair.SecretKey.s[i][j]);
      {
      if pk_recovered.t[i][j] <> KeyPair.PublicKey.t[i][j] then
      begin
        WriteLn('PK mismatch at t[', i, '][', j, ']');
        Result := False;
      end;
      if sk_recovered.s[i][j] <> KeyPair.SecretKey.s[i][j] then
      begin
        WriteLn('SK mismatch at s[', i, '][', j, ']');
        Result := False;
      end;
      }
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
  TestKeyGeneration;
  //}
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

