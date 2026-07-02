program keccak_test1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  CryptoUtils
;

procedure WriteSuccessOrFail(const a, b: String);
begin
  if a = b then
  begin
    WriteLn('SUCCESS');
  end
  else
  begin
    WriteLn('FAIL');
  end;
end;

procedure TestEmpty;
  var Keccak: TUKeccak;
  var Size: UInt32;
  var Output: TUInt8Array;
  var OutputStr: String;
begin
  WriteLn('Test Empty');
  Size := SizeOf(TUDigest_224);
  Keccak.Init(UDigestHashRate(Size), 1);
  Keccak.Absorb(nil, 0);
  Keccak.Finalize;
  Output := Keccak.Squeeze(Size);
  OutputStr := Output.ToHexLC;
  WriteLn('[', Length(Output), '] ', OutputStr);
  WriteSuccessOrFail(OutputStr, 'f71837502ba8e10837bdd8d365adb85591895602fc552b48b7390abd');
  WriteLn;
end;

procedure TestSHA3;
  var Keccak: TUKeccak;
  var Size: UInt32;
  var Input: TUInt8Array;
  var Output: TUInt8Array;
  var OutputStr: String;
begin
  WriteLn('Test SHA3');
  Input := UHexToBytes('ce9b3694398380585e4950fee58dbe9e65d537c04be7eeae7f81fca029');
  WriteLn('Input: ', Input.ToHexLC);
  Size := SizeOf(TUDigest_256);
  Keccak.Init(UDigestHashRate(Size), 6{SHA3});
  Keccak.Absorb(@Input[0], Length(Input));
  Keccak.Finalize;
  Output := Keccak.Squeeze(Size);
  OutputStr := Output.ToHexLC;
  WriteLn('[', Length(Output), '] ', OutputStr);
  WriteSuccessOrFail(OutputStr, '89fdb0cb95139c420df132bd42334af6a12b403673e31b97405499fdea566953');
  WriteLn;
end;

procedure TestSHAKE;
  var Keccak: TUKeccak;
  var Size: UInt32;
  var Input: TUInt8Array;
  var Output: TUInt8Array;
  var OutputStr: String;
begin
  WriteLn('Test SHAKE');
  Input := UHexToBytes('ce9b3694398380585e4950fee58dbe9e65d537c04be7eeae7f81fca029');
  WriteLn('Input: ', Input.ToHexLC);
  Size := SizeOf(TUDigest_256);
  Keccak.Init(UDigestHashRate(Size), $1f{SHAKE});
  Keccak.Absorb(@Input[0], Length(Input));
  Keccak.Finalize;
  Output := Keccak.Squeeze(Size);
  OutputStr := Output.ToHexLC;
  WriteLn('[', Length(Output), '] ', OutputStr);
  WriteSuccessOrFail(OutputStr, '3aa40c7fca7d56a2725f5dcd9dcc231408a3af41bfd064b951a076f2ce659342');
  WriteLn;
end;

procedure Run;
begin
  TestEmpty;
  TestSHA3;
  TestSHAKE;
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

