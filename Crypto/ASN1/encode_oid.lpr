program encode_oid;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

function EncodeOID(const s: String): TUInt8Array;
  function EncodeValue(const Value: UInt32): TUInt8Array;
    var v, n, i, j, p: UInt32;
    var b: UInt8;
    var Arr: TUInt8Array;
  begin
    Result := nil;
    Arr := nil;
    v := Value;
    n := 0;
    while v > 0 do
    begin
      b := (v mod 128) or n;
      n := $80;
      specialize UArrAppend<UInt8>(Arr, b);
      v := v div 128;
    end;
    SetLength(Result, Length(Arr));
    for i := 0 to High(Result) do
    begin
      Result[i] := Arr[High(Arr) - i];
    end;
  end;
  var OIDArrStr: TUStrArray;
  var OIDArr: TUInt32Array;
  var i: Int32;
begin
  OIDArrStr := UStrExplode(s, '.', False);
  SetLength(OIDArr, Length(OIDArrStr));
  for i := 0 to High(OIDArr) do
  begin
    OIDArr[i] := StrToInt(OIDArrStr[i]);
  end;
  Result := nil;
  specialize UArrAppend<UInt8>(Result, OIDArr[0] * 40 + OIDArr[1]);
  for i := 2 to High(OIDArr) do
  begin
    specialize UArrAppend<UInt8>(Result, EncodeValue(OIDArr[i]));
  end;
end;

function VerifyOID(const s: String): Boolean;
  var OIDArrStr: TUStrArray;
  var i, n: Int32;
begin
  OIDArrStr := UStrExplode(s, '.', False);
  if Length(OIDArrStr) < 3 then Exit(False);
  for i := 0 to High(OIDArrStr) do
  begin
    if not UStrIsNumber(OIDArrStr[i]) then Exit(False);
    n := StrToIntDef(OIDArrStr[i], 0);
    if n < 0 then Exit(False);
  end;
  Result := True;
end;

procedure Run;
  var s: String;
  var OID: TUInt8Array;
  var i: Int32;
begin
  WriteLn('Enter ASN1 OID:');
  ReadLn(s);
  //s := '1.2.840.113549.3.7';
  if not VerifyOID(s) then
  begin
    WriteLn('Invalid OID: ', s);
    Exit;
  end;
  OID := EncodeOID(s);
  Write('OID: ');
  for i := 0 to High(OID) do
  begin
    Write('$', IntToHex(OID[i], 2), ' ');
  end;
  WriteLn();
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

