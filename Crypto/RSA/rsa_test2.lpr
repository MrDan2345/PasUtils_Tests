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
  Key := UMakeRSAKey(4096, 24);
  //Key := UMakeRSAKey(2048, 24);

  Exp := UExportRSAKey_PKCS1(Key);
  WriteLn(Exp);
  Test := UImportRSAKey(Exp);
  CompareKeysPrivate(Key, Test, 'PKCS1 Private'); WriteLn();

  Exp := UExportRSAKey_PKCS8(Key);
  WriteLn(Exp);
  Test := UImportRSAKey(Exp);
  CompareKeysPrivate(Key, Test, 'PKCS8'); WriteLn();

  Exp := UExportRSAKey_X509(Key);
  WriteLn(Exp);
  Test := UImportRSAKey(Exp);
  CompareKeysPublic(Key, Test, 'X509'); WriteLn();

  Exp := UExportRSAKey_PKCS8(Key, 'MyPassword', 200000);
  WriteLn(Exp);
  Test := UImportRSAKey(Exp, 'MyPassword');
  CompareKeysPrivate(Key, Test, 'PKCS8 Encrypted'); WriteLn();
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

