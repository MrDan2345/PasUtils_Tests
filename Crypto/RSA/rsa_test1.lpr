program rsa_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils;

procedure Run;
  var TestStr: String = 'Hello World!';
  var Key, Temp: TURSA.TKey;
  var DecryptStr, Export: String;
  var Cipher: TUInt4096;
  var i, m: Int32;
  var Fails: array[0..1] of Int32;
begin
  m := 0;
  for m := 0 to 1 do
  begin
    Fails[m] := 0;
    case m of
      0: WriteLn('--- Testing PKCS1 padding ---');
      1: WriteLn('--- Testing OAEP padding ---');
    end;
    for i := 1 to 10 do
    begin
      WriteLn('Generating Key...');
      Key := UMakeRSAKey(2048, 24);
      Export := UExportRSAKey_PKCS1(Key);
      WriteLn(Export);
      Temp := UImportRSAKey(Export);
      if Key.n <> Temp.n then
      begin
        WriteLn('Mismatch!');
      end;
      case m of
        0:
        begin
          Cipher := UEncrypt_RSA_PKCS1_Str(TestStr, Key);
          DecryptStr := UDecrypt_RSA_PKCS1_Str(Cipher, Key);
        end;
        1:
        begin
          Cipher := UEncrypt_RSA_OAEP_Str(TestStr, Key);
          DecryptStr := UDecrypt_RSA_OAEP_Str(Cipher, Key);
        end;
      end;
      if TestStr = DecryptStr then
      begin
        WriteLn('SUCCESS');
      end
      else
      begin
        WriteLn('FAIL');
        Inc(Fails[m]);
      end;
      //WriteLn('Cipher: ', Cipher.ToBase64);
      WriteLn('Original:  ', TestStr);
      WriteLn('Decrypted: ', DecryptStr);
    end;
    WriteLn();
  end;
  WriteLn('PKCS1 Fails = ', Fails[0]);
  WriteLn('OAEP Fails = ', Fails[1]);
  WriteLn('Done.');
  //ReadLn;
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

