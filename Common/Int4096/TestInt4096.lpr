program TestInt4096;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils;

type TTestProc = procedure of Object;
type TTestProcArr = array of TTestProc;

type TDivModTester = class
public
  class procedure Simple;
  class procedure ZeroRemainder;
  class procedure Normalization;
  class procedure MultiWord;
  class procedure Boundary;
  class function MakeTests: TTestProcArr;
  class procedure DoTest;
end;

type TRSAKey = record
  n: TUInt4096; // The modulus
  e: TUInt4096; // The public exponent
  d: TUInt4096; // The private exponent
end;

function ModInverse(const A, N: TUInt4096): TUInt4096;
  var t, new_t, r, new_r, quotient, remainder: TUInt4096;
  var temp_t: TUInt4096;
begin
  t := TUInt4096.Zero;
  new_t := TUInt4096.One;
  r := N;
  new_r := A;
  while not new_r.IsZero do
  begin
    quotient := TUInt4096.DivisionModular(r, new_r, remainder);
    r := new_r;
    new_r := remainder;
    temp_t := t;
    t := new_t;
    new_t := temp_t - (quotient * new_t);
  end;
  if r > TUInt4096.One then Exit(TUInt4096.Zero);
  quotient := TUInt4096.DivisionModular(t, N, Result);
end;

function GenerateRSAKey(const KeySizeInBits: Int32 = 2048): TRSAKey;
  var p, q, n, phi, e, d: TUInt4096;
  var PrimeSizeInBits: Int32;
begin
  PrimeSizeInBits := KeySizeInBits shr 1;
  e := 65537;
  WriteLn('Generating RSA Key Pair...');
  repeat
    WriteLn('Generating prime p...');
    p := TUInt4096.MakePrime(PrimeSizeInBits);
    WriteLn(p.ToString);
    WriteLn('Generating prime q...');
    q := TUInt4096.MakePrime(PrimeSizeInBits);
    WriteLn(q.ToString);
    if p = q then Continue;
    n := p * q;
    phi := (p - TUInt4096.One) * (q - TUInt4096.One);
  until TUInt4096.GCD(e, phi) = TUInt4096.One;
  WriteLn('Primes found. Calculating private exponent d...');
  d := ModInverse(e, phi);
  Result.n := n;
  Result.e := e;
  Result.d := d;
  WriteLn('Key Modulus: ', n.ToString);
  WriteLn('Key Public: ', e.ToString);
  WriteLn('Key Private: ', d.ToString);
  WriteLn('RSA Key Pair Generation Complete.');
end;

procedure TestMod;
  function IntPow(const Base: UInt64; const Exponent: UInt32): UInt64;
    var i: UInt32;
  begin
    if Exponent = 0 then Exit(1);
    Result := Base;
    for i := 2 to Exponent do
    begin
      Result *= Base;
    end;
  end;
  var a, b, Actual, Expected: TUInt4096;
begin
  WriteLn('--- Testing Mod with 2^32 mod 7 ---');
  a := TUInt4096.One shl 2048;
  b := 7;
  Actual := a mod b;
  Expected := 4;//IntPow(2, 64) mod 7;//4;
  if Actual = Expected then
  begin
    WriteLn('SUCCESS: Mod works!');
    WriteLn('Result: ', Actual.ToString);
  end
  else
  begin
    WriteLn('FAIL: Mod returned the wrong result.');
    WriteLn('Expected: ', Expected.ToString);
    WriteLn('Actual:   ', Actual.ToString);
  end;
end;

procedure TestDiv;
  var Dividend, Divider, Remainder, Result: TUInt4096;
begin
  WriteLn('--- Testing Div ---');
  //Dividend := '$800000';
  Dividend := TUInt4096.One shl 64;
  Divider := 7;
  Result := TUInt4096.Division(Dividend, Divider, Remainder);
  WriteLn('Dividend: ', Dividend.ToHex);
  WriteLn('Divider: ', Divider.ToString);
  WriteLn('Result: ', Result.ToHex);
  WriteLn('Remainder: ', Remainder.ToString);
end;

procedure TestPowMod_Simple;
var
  Base, Exponent, Modulus, Expected, Actual: TUInt4096;
begin
  WriteLn('--- Testing PowMod with 3^6 mod 7 ---');
  Base := 3;
  Exponent := 6;
  Modulus := 7;
  Expected := 1;
  Actual := TUInt4096.PowMod(Base, Exponent, Modulus);
  if Actual = Expected then
  begin
    WriteLn('SUCCESS: PowMod works for the simple test case!')
  end
  else
  begin
    WriteLn('FAIL: PowMod returned the wrong result.');
    WriteLn('Expected: ', Expected.ToString);
    WriteLn('Actual:   ', Actual.ToString);
  end;
end;

procedure TestMontMult_Simple;
var
  Context: TUInt4096Impl.TMontgomeryReduction.TContext;
  InputA, InputB, Expected, Actual: TUInt4096;
begin
  WriteLn('--- Testing MontMult with (3*5) mod 7 ---');
  // 1. Initialize the context for N=7. We know this works.
  Context := TUInt4096.TMontgomeryReduction.InitContext(TUInt4096.Make(7));

  // 2. Set up the inputs and expected output based on our math.
  InputA := 5; // This is 3 in Montgomery form
  InputB := 6; // This is 5 in Montgomery form
  Expected := 4; // This is (3*5) in Montgomery form

  // 3. Run your MontMult function
  Actual := TUInt4096.TMontgomeryReduction.MontMult(Context, InputA, InputB);

  // 4. Check the result
  if Actual = Expected then
  begin
    WriteLn('SUCCESS: MontMult works for the simple test case!');
  end
  else
  begin
    WriteLn('FAIL: MontMult returned the wrong result.');
    WriteLn('Expected: ', Expected.ToString);
    WriteLn('Actual:   ', Actual.ToString);
  end;
end;

procedure TestShl;
  var Mask: TUInt4096;
  var i: Int32;
  var IsCorrect: Boolean;
begin
  WriteLn('--- Testing (One shl 1023) ---');
  Mask := TUInt4096.One shl 1023;
  IsCorrect := True;
  if Mask[31] <> $80000000 then
  begin
    WriteLn('FAIL: Word[31] is incorrect. Expected $80000000, got $' + IntToHex(Mask[31], 8));
    IsCorrect := False;
  end;
  for i := 0 to High(Mask.MaxItem) do
  begin
    if (i <> 31) and (Mask[i] <> 0) then
    begin
      WriteLn('FAIL: Word[' + IntToStr(i) + '] should be 0, but is $' + IntToHex(Mask[i], 8));
      IsCorrect := False;
    end;
  end;
  if IsCorrect then
  begin
    WriteLn('SUCCESS: The bitmask for MSB is generated correctly!');
  end;
end;

function PackBlock(const Data: TUInt8Array): TUInt4096;
  const BlockSize = 256;
  const MinPadding = 11;
  var PaddingSize: Int32;
  var PaddedData: array[0..BlockSize - 1] of UInt8 absolute Result;
  var i: Int32;
begin
  if Length(Data) > BlockSize - MinPadding then Exit(TUInt4096.Invalid);
  Result := TUInt4096.Zero;
  PaddingSize := BlockSize - Length(Data);
  PaddedData[0] := $00;
  PaddedData[1] := $02;
  for i := 2 to PaddingSize - 2 do
  repeat
    PaddedData[i] := UInt8(Random(256));
  until PaddedData[i] > 0;
  PaddedData[PaddingSize - 1] := 0;
  for i := 0 to High(Data) do
  begin
    PaddedData[PaddingSize + i] := Data[i];
  end;
end;

function UnpackBlock(const Block: TUInt4096): TUInt8Array;
  const BlockSize = 256;
  var PaddingSize: Int32;
  var PaddedData: array[0..BlockSize - 1] of UInt8 absolute Block;
  var i: Int32;
begin
  if (PaddedData[0] <> 0) or (PaddedData[1] <> 2) then Exit(nil);
  PaddingSize := 0;
  for i := 2 to High(PaddedData) do
  begin
    if PaddedData[i] <> 0 then Continue;
    PaddingSize := i + 1;
    Break;
  end;
  if PaddingSize = 0 then Exit(nil);
  Result := nil;
  SetLength(Result, BlockSize - PaddingSize);
  for i := 0 to High(Result) do
  begin
    Result[i] := PaddedData[PaddingSize + i];
  end;
end;

function StrToBlock(const Str: String): TUInt4096;
  var Data: TUInt8Array;
begin
  SetLength(Data, Length(Str));
  Move(Str[1], Data[0], Length(Str));
  Result := PackBlock(Data);
end;

function BlockToStr(const Block: TUInt4096): String;
  var Data: TUInt8Array;
begin
  Data := UnpackBlock(Block);
  SetLength(Result, Length(Data));
  Move(Data[0], Result[1], Length(Result));
end;

function Encrypt(const Data: TUInt8Array; const Key: TRSAKey): TUInt4096;
  var Block: TUInt4096;
begin
  Block := PackBlock(Data);
  if not Block.IsValid then Exit(TUInt4096.Invalid);
  Result := TUInt4096.PowMod(Block, Key.e, Key.n);
end;

function Decrypt(const Cipher: TUInt4096; const Key: TRSAKey): TUInt8Array;
  var Block: TUInt4096;
begin
  Block := TUInt4096.PowMod(Cipher, Key.d, Key.n);
  Result := UnpackBlock(Block);
end;

function EncryptStr(const Str: String; const Key: TRSAKey): TUInt4096;
  var Block: TUInt4096;
begin
  Block := StrToBlock(Str);
  if not Block.IsValid then Exit(TUInt4096.Invalid);
  Result := TUInt4096.PowMod(Block, Key.e, Key.n);
end;

function DecryptStr(const Cipher: TUInt4096; const Key: TRSAKey): String;
  var Block: TUInt4096;
begin
  Block := TUInt4096.PowMod(Cipher, Key.d, Key.n);
  Result := BlockToStr(Block);
end;

procedure Run;
  var n, n1, n2, r: TUInt4096;
  var i, j, k: Int32;
  var s: String;
  var Key: TRSAKey;
begin
  Randomize;
  Key := GenerateRSAKey;
  n := EncryptStr('Hello World!', Key);
  WriteLn(n.ToString);
  s := DecryptStr(n, Key);
  WriteLn(s);
  Exit;
  //i := 10;
  //j := -7;
  //WriteLn(i div j, ':', i mod j);
  //GenerateRSAKey;
  //TestShl;
  //TestPowMod_Simple;
  //TestMontMult_Simple;
  //TestMod;
  //TestDiv;
  //TDivModTester.DoTest;
  Exit;
  //n := TUInt4096.Make('$f9a3409c3b4433f');
  {n := TUInt4096.Make(
    '1039456237331496739069977513049273195498020105219957891323018612243649948'+
    '0798871083325064400272241103834179365093503020151733143620283051637313572'+
    '1646977755056500547935901789236815057665522946481916017844605944091285242'+
    '2512881243218522680016369411273656167838575943646578907473796586771386026'+
    '6846933348640260868518072734796607817506948425519838212558800426886825899'+
    '6001072672261496007728465483225688668126803144308845382121179417384205576'+
    '1580290687211715054036282733028312430102954044150342802362376277504298613'+
    '2074247323165474819050098517207257735123077580733865126978681777448040042'+
    '6090002449513190686132620116152142593569803308763974009151039817486236333'+
    '8067881921919554275559227433756669625675031636859491458971654417103151695'+
    '8170748152494948092776477894193668350957732612248187136809483848962198619'+
    '9361977892127725557126713280651824010487654025168128150748892553086614965'+
    '6525219014935945094173636643309014056537148948819609387803188016054871449'+
    '0758584795301990508335270888258553043281143058727182665995609698533508494'+
    '3884138668601363401948429070547369660951787170118900772230074535797478984'+
    '0393585218214191091586749451982674746752737271939695100136047252875302877'+
    '937798199791066531930753322433821143090810726015709045892318909912'
  );}
  //n := TUInt4096.MakeRandom();
  n := TUInt4096.MakePrime(2048);
  //n := TUInt4096.Make(-39485391235234123);
  //n1 := TUInt4096.MakeRandom(256);
  //n2 := TUInt4096.MakeRandom(256);
  //n1 := TUInt4096.Make('8934562983456293');
  //n2 := TUInt4096.Make('8912648746123');
  //n1 := TUInt4096.Make('79630621571658699799223702039');
  //n1 := TUInt4096.Make('796306215716586997992982374623702039');
  //n1 := TUInt4096.MakeRandom(4095);
  //n2 := TUInt4096.Make('8912648746123');
  //n2 := TUInt4096.Make('89126019834748746123');
  //n2 := TUInt4096.MakeRandom(16);
  //n := TUInt4096.Subtraction(n1, n2);
  //n := TUInt4096.Multiplication(n1, n2);
  //n := TUInt4096.Division(n1, n2, r);
  //WriteLn(n1.ToString, ' - ', n2.ToString, ' = ', n.ToString);
  //WriteLn(n1.ToString, ' * ', n2.ToString, ' = ', n.ToString);
  //WriteLn(n1.ToString, ' / ', n2.ToString, ' = ', n.ToString, '; r = ', r.ToString); //8934562983456293
  //WriteLn(n.ToHex);
  WriteLn(n.ToString);
end;

{ TDivModTester }

class procedure TDivModTester.Simple;
  var Dividend, Divisor, Quotient, Remainder, ExpectedQ, ExpectedR: TUInt4096;
begin
  WriteLn('Test Simple');
  WriteLn('--- Testing: 100 / 13 ---');
  Dividend := 100;
  Divisor  := 13;
  ExpectedQ := 7;
  ExpectedR := 9;
  Quotient := TUInt4096.Division(Dividend, Divisor, Remainder);
  if (Quotient = ExpectedQ) and (Remainder = ExpectedR) then
  begin
    WriteLn('SUCCESS');
  end
  else
  begin
    WriteLn('FAIL');
  end;
end;

class procedure TDivModTester.ZeroRemainder;
  var Dividend, Divisor, Quotient, Remainder, ExpectedQ, ExpectedR: TUInt4096;
begin
  WriteLn('Test Zero Remainder');
  WriteLn('--- Testing: (2^64) / (2^32) ---');
  Dividend := TUInt4096.One shl 64;
  Divisor  := TUInt4096.One shl 32;
  ExpectedQ := TUInt4096.One shl 32;
  ExpectedR := TUInt4096.Zero;
  Quotient := TUInt4096.Division(Dividend, Divisor, Remainder);
  if (Quotient = ExpectedQ) and (Remainder = ExpectedR) then
  begin
    WriteLn('SUCCESS');
  end
  else
  begin
    WriteLn('FAIL');
  end;
end;

class procedure TDivModTester.Normalization;
var
  Dividend, Divisor, Quotient, Remainder, ExpectedQ, ExpectedR: TUInt4096;
begin
  WriteLn('Test Normalization');
  WriteLn('--- Testing: (2^65 + 1) / (2^31) ---');
  Dividend := TUInt4096.One shl 65;
  Dividend := Dividend + TUInt4096.One; // Assuming you have an Add function
  Divisor  := TUInt4096.One shl 31;
  ExpectedQ := TUInt4096.One shl 34;
  ExpectedR := TUInt4096.One;
  Quotient := TUInt4096.Division(Dividend, Divisor, Remainder);
  if (Quotient = ExpectedQ) and (Remainder = ExpectedR) then
  begin
    WriteLn('SUCCESS');
  end
  else
  begin
    WriteLn('FAIL');
  end;
end;

class procedure TDivModTester.MultiWord;
  var Dividend, Divisor, Quotient, Remainder, ExpectedQ, ExpectedR: TUInt4096;
begin
  WriteLn('Test MultiWord');
  WriteLn('--- Testing: (2^96 + 5) / (2^32 + 3) ---');
  Dividend := (TUInt4096.One shl 96) + TUInt4096.Make(5);
  Divisor  := (TUInt4096.One shl 32) + TUInt4096.Make(3);
  ExpectedQ := TUInt4096.Zero;
  ExpectedQ[1] := $FFFFFFFD;
  ExpectedQ[0] := 8;
  ExpectedR := TUInt4096.Zero;
  ExpectedR[0] := $FFFFFFED;
  Quotient := TUInt4096.Division(Dividend, Divisor, Remainder);
  if (Quotient = ExpectedQ) and (Remainder = ExpectedR) then
  begin
    WriteLn('SUCCESS');
  end
  else
  begin
    WriteLn('FAIL');
  end;
end;

class procedure TDivModTester.Boundary;
  var Dividend, Divisor, Quotient, Remainder, ExpectedQ, ExpectedR: TUInt4096;
  var i: Integer;
begin
  WriteLn('Test Boundary');
  WriteLn('--- Testing: (2^4095 - 1) / (2^2048 - 1) ---');
  // Create Dividend = 2^4095 - 1
  Dividend := TUInt4096.One shl 4095;
  Dividend := Dividend - TUInt4096.One;
  // Create Divisor = 2^2048 - 1
  Divisor := TUInt4096.One shl 2048;
  Divisor := Divisor - TUInt4096.One;
  // Create ExpectedQ = 2^2047
  ExpectedQ := TUInt4096.One shl 2047;
  // Create ExpectedR = 2^2047 - 1
  ExpectedR := ExpectedQ - TUInt4096.One;
  Quotient := TUInt4096.Division(Dividend, Divisor, Remainder);
  if (Quotient = ExpectedQ) and (Remainder = ExpectedR) then
  begin
    WriteLn('SUCCESS');
  end
  else
  begin
    WriteLn('FAIL');
  end;
end;

class function TDivModTester.MakeTests: TTestProcArr;
begin
  Result := [
    @Simple,
    @ZeroRemainder,
    @Normalization,
    @MultiWord,
    @Boundary
  ];
end;

class procedure TDivModTester.DoTest;
  var i: Int32;
  var Tests: TTestProcArr;
begin
  Tests := MakeTests;
  for i := 0 to High(Tests) do
  begin
    Tests[i]();
    WriteLn('');
  end;
end;

begin
  Run;
end.

