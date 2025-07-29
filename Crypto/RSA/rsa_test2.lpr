program rsa_test2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

procedure CompareKeys(const a, b: TURSA.TKey; const HighCmp: Int32; const LabelStr: String);
  var CmpA: array[0..7] of TUInt4096 absolute a;
  var CmpB: array[0..7] of TUInt4096 absolute b;
  var i: Int32;
  var Result: Boolean;
begin
  Result := True;
  for i := 0 to HighCmp do if CmpA[i] <> CmpB[i] then Result := False;
  if Result then
  begin
    WriteLn(LabelStr, ' SUCCESS');
  end
  else
  begin
    WriteLn(LabelStr, ' FAIL');
  end;
end;

procedure CompareKeysPrivate(const a, b: TURSA.TKey; const LabelStr: String);
begin
  CompareKeys(a, b, 7, LabelStr);
end;

procedure CompareKeysPublic(const a, b: TURSA.TKey; const LabelStr: String);
begin
  CompareKeys(a, b, 1, LabelStr);
end;

procedure Run;
  var Key, Test: TURSA.TKey;
  var Exp: String;
begin
  WriteLn('Making RSA Key...');
  Key := UMakeRSAKey(2048, 24);

  Exp := TURSA.ExportKeyPrivate_PKCS1(Key);
  WriteLn(Exp);
  Test := TURSA.ImportKeyPrivate_PKCS1(Exp);
  CompareKeysPrivate(Key, Test, 'PKCS1 Private'); WriteLn();

  Exp := TURSA.ExportKeyPublic_PKCS1(Key);
  WriteLn(Exp);
  Test := TURSA.ImportKeyPublic_PKCS1(Exp);
  CompareKeysPublic(Key, Test, 'PKCS1 Public'); WriteLn();

  Exp := TURSA.ExportKeyPrivate_PKCS8(Key);
  WriteLn(Exp);
  Test := TURSA.ImportKeyPrivate_PKCS8(Exp);
  CompareKeysPrivate(Key, Test, 'PKCS8'); WriteLn();

  Exp := TURSA.ExportKeyPublic_X509(Key);
  WriteLn(Exp);
  Test := TURSA.ImportKeyPublic_X509(Exp);
  CompareKeysPublic(Key, Test, 'X509'); WriteLn();
end;

begin
  Run;
end.

