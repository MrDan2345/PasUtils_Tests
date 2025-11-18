program ecc_montgomery;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CryptoUtils,
  CommonUtils;

function SwapHex(const Hex: String): String;
  var i, j: Int32;
begin
  SetLength(Result, Length(Hex));
  for i := 0 to Length(Hex) div 2 - 1 do
  begin
    j := i * 2;
    Result[j + 1] := Hex[Length(Hex) - j - 1];
    Result[j + 2] := Hex[Length(Hex) - j];
  end;
end;

procedure Test;
  var Nums: array of String = (
    '0100000000000000000000000000000000000000000000000000000000000000',
    '57119fd0dd4e22d8868e1c58c45c4404fb5e83c95b1d0b124c80a5bcbc959c5f',
    '7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec'
  );
  var i: Int32;
begin
  for i := 0 to High(Nums) do
  begin
    WriteLn(Nums[i]);
    WriteLn(SwapHex(Nums[i]));
  end;
end;

begin
  Test;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

