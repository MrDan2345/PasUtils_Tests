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
  var Key: TURSA.TKey;
  var TestStr: String = 'Hello World!';
  var DecryptStr: String;
  var Cipher: TUInt4096;
begin
  Randomize;
  WriteLn('Generating Key...');
  Key := UMakeRSAKey(2048, 24);
  WriteLn('Key Modulus: ', Key.n.ToString);
  WriteLn('Key Public: ', Key.e.ToString);
  WriteLn('Key Private: ', Key.d.ToString);
  Cipher := UEncrypt_RSA_Str(TestStr, Key);
  DecryptStr := UDecrypt_RSA_Str(Cipher, Key);
  if TestStr = DecryptStr then
  begin
    WriteLn('SUCCESS');
  end
  else
  begin
    WriteLn('FAIL');
  end;
  WriteLn('Cipher: ', Cipher.ToString);
  WriteLn('Original:  ', TestStr);
  WriteLn('Decrypted: ', DecryptStr);
  //ReadLn;
end;

begin
  Run;
end.

