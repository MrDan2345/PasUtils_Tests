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

procedure DivMod_Reference(const Dividend, Divisor: TUInt4096; var Quotient, Remainder: TUInt4096);
var
  CurrentDividend: TUInt4096;
  i: Integer;
  one: TUInt4096;
begin
  Quotient := TUInt4096.Zero;
  CurrentDividend := TUInt4096.Zero;
  one := TUInt4096.One;
  for i := (Dividend.Top * 32 + 31) downto 0 do
  begin
    // Shift the current dividend left by 1
    CurrentDividend := TUInt4096.ShiftLeft(CurrentDividend, 1);
    // Bring down the next bit from the original dividend
    if (TUInt4096.ShiftLeft(one, i) and Dividend) > TUInt4096.Zero then
    begin
      CurrentDividend[0] := CurrentDividend[0] or 1;
    end;
    // If the current dividend is >= the divisor, subtract and set quotient bit
    if TUInt4096.Compare(CurrentDividend, Divisor) >= 0 then
    begin
      CurrentDividend := TUInt4096.Subtraction(CurrentDividend, Divisor);
      Quotient := Quotient or TUInt4096.ShiftLeft(one, i);
    end;
  end;
  Remainder := CurrentDividend;
end;

function Multiply_Reference(const a, b: TUInt4096): TUInt4096;
  var LocalA, LocalB: TUInt4096;
begin
  Result := TUInt4096.Zero;
  LocalA := a;
  LocalB := b;
  while not LocalB.IsZero do
  begin
    // If b is odd, add a to the result
    if LocalB.IsOdd then
    begin
      Result := TUInt4096.Addition(Result, LocalA);
    end;
    // Double a
    LocalA := TUInt4096.ShiftLeft(LocalA, 1);
    // Halve b
    LocalB := TUInt4096.ShiftRight(LocalB, 1);
  end;
  Result.SetNegative(a.IsNegative xor b.IsNegative);
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
    quotient := TUInt4096.Division(r, new_r, remainder);
    r := new_r;
    new_r := remainder;
    temp_t := t;
    t := new_t;
    new_t := temp_t - (quotient * new_t);
  end;
  if r > TUInt4096.One then Exit(TUInt4096.Zero);
  quotient := TUInt4096.Division(t, N, Result);
end;

// A reference version of ModInverse that uses the slow, trusted division.
function ModInverse_Reference(const A, N: TUInt4096): TUInt4096;
  var t, new_t, r, new_r, quotient, remainder: TUInt4096;
  var temp_t, prod: TUInt4096;
  var i: Int32;
begin
  t := TUInt4096.Zero;
  new_t := TUInt4096.One;
  r := N;
  new_r := A;
  WriteLn('--- Reference Quotients ---');
  while not (new_r.IsZero) do
  begin
    DivMod_Reference(r, new_r, quotient, remainder);
    //WriteLn('DivMod ', r.ToString, ' ', new_r.ToString, ' ', quotient.ToString, ' ', remainder.ToString);
    //WriteLn('  Ref Q: ', quotient.ToString, '; R: ', remainder.ToString); // Print quotient
    r := new_r;
    new_r := remainder;
    temp_t := t;
    t := new_t;
    prod := Multiply_Reference(quotient, new_t);
    WriteLn('Mul ', quotient.ToString, ' ', new_t.ToString, ' ', prod.ToString);
    new_t := TUInt4096.Subtraction(temp_t, prod);
    //WriteLn('Sub ', r.ToString, ' ', new_r.ToString, ' ', quotient.ToString, ' ', remainder.ToString);
  end;
  if TUInt4096.Compare(r, TUInt4096.One) > 0 then Exit(TUInt4096.Zero);
  DivMod_Reference(t, N, quotient, Result);
end;

function ModInverse2(const e, phi: TUInt4096): TUInt4096;
  function gcd(const a, b: TUInt4096; var x: TUInt4096; var y: TUInt4096): TUInt4096;
    var x1, y1, gcd_val: TUInt4096;
  begin
    if a = TUInt4096.Zero then
    begin
      x := TUInt4096.Zero;
      y := TUInt4096.One;
      Exit(b);
    end;
    x1 := TUInt4096.Zero;
    y1 := TUInt4096.Zero;
    gcd_val := gcd(b mod a, a, x1, y1);
    x := y1 - (b div a) * x1;
    y := x1;
    Result := gcd_val;
  end;
  var x, y, gcd_val: TUInt4096;
begin
  x := TUInt4096.Zero;
  y := TUInt4096.Zero;
  gcd_val := gcd(e, phi, x, y);
  if gcd_val <> TUInt4096.One then
  begin
    Exit(TUInt4096.Invalid);
  end;
  if x < TUInt4096.Zero then
  begin
    x := x + phi;
  end;
  Result := x;
end;

// A version of your ModInverse that prints its quotients for comparison.
function ModInverse_Fast_WithDebug(const A, N: TUInt4096): TUInt4096;
  var t, new_t, r, new_r, quotient, remainder: TUInt4096;
  var temp_t: TUInt4096;
begin
  t := TUInt4096.Zero;
  new_t := TUInt4096.One;
  r := N;
  new_r := A;
  WriteLn('--- Fast Quotients ---');
  while not (new_r.IsZero) do
  begin
    quotient := TUInt4096.Division(r, new_r, remainder); // Your fast DivMod
    WriteLn('  Fast Q: ', quotient.ToString, '; R: ', remainder.ToString); // Print quotient
    r := new_r;
    new_r := remainder;
    temp_t := t;
    t := new_t;
    new_t := TUInt4096.Subtraction(temp_t, TUInt4096.Multiplication(quotient, new_t));
  end;
  if TUInt4096.Compare(r, TUInt4096.One) > 0 then Exit(TUInt4096.Zero);
  quotient := TUInt4096.Division(t, N, Result);
end;

function VerifyKeyPair(const d, e, phi: TUInt4096): Boolean;
  var d_e, check: TUInt4096;
begin
  // Calculate (d * e) mod phi
  d_e := d * e;
  check := d_e mod phi;// check mod phi;
  Result := TUInt4096.Compare(check, TUInt4096.One) = 0;
end;

function GenerateRSAKey_Debug(const KeySizeInBits: Int32 = 2048): TRSAKey;
  var p, q, n, phi, phi_test, e, d, test_n, p_min_1, q_min_1, p_min_1_test, q_min_1_test: TUInt4096;
  var PrimeSizeInBits: Int32;
  var IsKeyValid: Boolean;
begin
  PrimeSizeInBits := KeySizeInBits shr 1;
  e := 65537;
  WriteLn('Generating RSA Key Pair...');
  IsKeyValid := False;
  repeat
    repeat
      WriteLn('Generating prime p...');
      p := TUInt4096.MakePrime(PrimeSizeInBits);
      //p := '90382099633927408949568785463586054952142834624445251623288095609611016768219411333537389178145143450387852210769990218700330811847585011060673764606881801376567727990096387434077512196441872418045680023804065905929228854720360338660469402853809932678152144289496776610253459060449467689793626996923806670363';
      WriteLn(p.ToString);
      WriteLn('Generating prime q...');
      q := TUInt4096.MakePrime(PrimeSizeInBits);
      //q := '105985773170443866860819656797185778690784978271262571811743419344359058013545821935233836842049632863832031309415574359841271266239653153294916058029909703725420707498060002779155798668464771073302245038158220861914717093568858369920259151475759819817591423325646504713070790995329876121813729903323454753711';
      WriteLn(q.ToString);
      if p = q then Continue;
      n := TUInt4096.Multiplication(p, q);
      //n := Multiply_Reference(p, q);
      WriteLn('Modulus:');
      WriteLn(n.ToString);
      test_n := '9579216710469888020013626076243276790781202445636401798936690617498346456563861300120676016669143862068011213481767206376391994431400411641936892471993063559808302879160490942405249760243933007325297038561324554359677714054049082667310933299060466593424566224497283691914262896032483857395680592507072883074197927623007462418542535972633213568987024313874236835319519069224112303083383336553397701919691128299556705720461511537243810245376100151028780304983824770214578098726229472661391284895658962039348908230014325772300957345767789379422938968393169625383690058332502615768644276946512370895627405983843127967093';
      if test_n = n then
      begin
        WriteLn('Modulus match');
      end
      else
      begin
        WriteLn('Modulus mismatch');
      end;
      p_min_1 := TUInt4096.Subtraction(p, TUInt4096.One);
      p_min_1_test := '90382099633927408949568785463586054952142834624445251623288095609611016768219411333537389178145143450387852210769990218700330811847585011060673764606881801376567727990096387434077512196441872418045680023804065905929228854720360338660469402853809932678152144289496776610253459060449467689793626996923806670362';
      if p_min_1_test = p_min_1 then
      begin
        WriteLn('p - 1 match');
      end
      else
      begin
        WriteLn('p - 1 mismatch');
      end;
      q_min_1 := TUInt4096.Subtraction(q, TUInt4096.One);
      q_min_1_test := '105985773170443866860819656797185778690784978271262571811743419344359058013545821935233836842049632863832031309415574359841271266239653153294916058029909703725420707498060002779155798668464771073302245038158220861914717093568858369920259151475759819817591423325646504713070790995329876121813729903323454753710';
      if q_min_1_test = q_min_1 then
      begin
        WriteLn('q - 1 match');
      end
      else
      begin
        WriteLn('q - 1 mismatch');
      end;
      phi := Multiply_Reference(p_min_1, q_min_1);
      phi_test := '9579216710469888020013626076243276790781202445636401798936690617498346456563861300120676016669143862068011213481767206376391994431400411641936892471993063559808302879160490942405249760243933007325297038561324554359677714054049082667310933299060466593424566224497283691914262896032483857395680592507072883074001559750203091142732147530372441735344096500978529011884487554270142228301618103284626475899496351985336822200275946958702208167288861986673190482347033265112589663238073082448157974030752318548000983168052039004457011397478570670842210414063599872887946490717359334445320026890733027084020049083595866543020';
      if phi_test = phi then
      begin
        WriteLn('phi match');
      end
      else
      begin
        WriteLn('phi mismatch');
      end;
      WriteLn('Phi:');
      WriteLn(phi.ToString);
    until TUInt4096.GCD(e, phi) = TUInt4096.One;
    WriteLn('Primes found. Calculating private exponent d...');
    d := ModInverse2(e, phi);//ModInverse_Reference(e, phi);
    IsKeyValid := VerifyKeyPair(d, e, phi);
    if not IsKeyValid then
    begin
      WriteLn('!!! FAILED KEY VERIFICATION !!!');
      WriteLn('ModInverse produced an incorrect result for phi:');
      WriteLn('phi = ', phi.ToString);
      Continue; // Try again with a new p and q
    end;
  until IsKeyValid;
  Result.n := n;
  Result.e := e;
  Result.d := d;
  WriteLn('Key Modulus: ', n.ToString);
  WriteLn('Key Public: ', e.ToString);
  WriteLn('Key Private: ', d.ToString);
  WriteLn('RSA Key Pair Generation Complete.');
end;

function GenerateRSAKey(const KeySizeInBits: Int32 = 2048): TRSAKey;
  var p, q, n, phi, phi_test, e, d, test_n, p_min_1, q_min_1, p_min_1_test, q_min_1_test: TUInt4096;
  var PrimeSizeInBits: Int32;
  var IsKeyValid: Boolean;
begin
  PrimeSizeInBits := KeySizeInBits shr 1;
  e := 65537;
  WriteLn('Generating RSA Key Pair...');
  IsKeyValid := False;
  repeat
    repeat
      WriteLn('Generating prime p...');
      p := TUInt4096.MakePrime(PrimeSizeInBits);
      //WriteLn(p.ToString);
      WriteLn('Generating prime q...');
      q := TUInt4096.MakePrime(PrimeSizeInBits);
      //WriteLn(q.ToString);
      if p = q then Continue;
      n := TUInt4096.Multiplication(p, q);
      //WriteLn('Modulus:');
      //WriteLn(n.ToString);
      p_min_1 := TUInt4096.Subtraction(p, TUInt4096.One);
      q_min_1 := TUInt4096.Subtraction(q, TUInt4096.One);
      phi := TUInt4096.Multiplication(p_min_1, q_min_1);
      //WriteLn('Phi:');
      //WriteLn(phi.ToString);
    until TUInt4096.GCD(e, phi) = TUInt4096.One;
    WriteLn('Primes found. Calculating private exponent d...');
    d := ModInverse2(e, phi);//ModInverse_Reference(e, phi);
    IsKeyValid := VerifyKeyPair(d, e, phi);
    if not IsKeyValid then
    begin
      WriteLn('!!! FAILED KEY VERIFICATION !!!');
      WriteLn('ModInverse produced an incorrect result for phi:');
      WriteLn('phi = ', phi.ToString);
      Continue;
    end;
  until IsKeyValid;
  Result.n := n;
  Result.e := e;
  Result.d := d;
  WriteLn('Key Modulus: ', n.ToString);
  WriteLn('Key Public: ', e.ToString);
  WriteLn('Key Private: ', d.ToString);
  WriteLn('RSA Key Pair Generation Complete.');
end;

procedure TestModInverse;
  var d, e, phi: TUInt4096;
begin
  e := 65537;
  phi := '18405040002732956912924006895823546571601767193002177106151131523982799323668173299271430457366649445437296021126139106824963650547431034261222369585730432641952115042625168358902412650562294411661145143145371835374013901191324550835314973801839894312973361486957082294169589750181678171485968476347838973141815597189491442301035398402631981039065811172023583441414639943487946402203151344827969370590453234666203047234151408390211263016101186945733813554241968543372355585601894220708355244634313311019083756261660458464172252085631531220117354116535509502035516115589353190516893117330385008135054683759643996604544';
  d := ModInverse(e, phi);
  if VerifyKeyPair(d, e, phi) then
  begin
    WriteLn('ModInverse Successful');
  end
  else
  begin
    WriteLn('ModInverse Failed');
  end;
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
  //Actual := a mod b;
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

procedure TestMultiplication;
  var i, err: Int32;
  var a, b, r0, r1: TUInt4096;
begin
  err := 0;
  for i := 0 to 999 do
  begin
    a := TUInt4096.MakeRandom(2048);
    b := TUInt4096.MakeRandom(2048);
    r0 := TUInt4096.Multiplication(a, b);
    r1 := Multiply_Reference(a, b);
    if r0 = r1 then Continue;
    Inc(err);
  end;
  WriteLn('Multiplication Errors: ', err);
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
  //const BlockSize = 128;
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
  //const BlockSize = 128;
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

procedure TestVerificationFailure;
var
  // --- PASTE THE FAILING VALUES HERE ---
  // You will get these from the output of your key generation loop
  phi, d, e, one: TUInt4096;

  // --- Variables for the test ---
  temp_fast, check_fast: TUInt4096;
  temp_ref, check_ref, dummy_q: TUInt4096;
begin
  WriteLn('--- Testing the Failing Verification Step ---');

  // Initialize the known failing numbers and constants
  e := 65537;
  one := TUInt4096.One;

  // Example failing phi from your output (replace with a new one if needed)
  phi := '94592757808584081709006454738865617093549070117015920124141068278231457657804858549566844415331841551904698205757807164772525224195932245751432883005579293303325894980116127256455427035577122316562356318798206615011112800197909701242613779706486930438919912060943758600975036862046675515916255316905103752172';

  // Calculate 'd' using the trusted reference ModInverse to be sure it's correct
  d := ModInverse_Reference(e, phi); // Assuming you have this from the previous debug step

  WriteLn('Testing with known correct d: ', d.ToString);
  WriteLn;

  // --- STEP 1: Test the Multiplication ---
  WriteLn('Testing Multiplication...');
  temp_fast := TUInt4096.Multiplication(d, e);
  temp_ref := Multiply_Reference(d, e);
  if temp_fast = temp_ref then
  begin
    WriteLn('Multiplication check passed!');
  end
  else
  begin
    WriteLn('Multiplication check failed!');
    WriteLn('Fast: ', temp_fast.ToString);
    WriteLn('Reference: ', temp_ref.ToString);
  end;

  // --- STEP 2: Test the Modulo ---
  WriteLn('Testing Modulo...');
  // Your fast version
  dummy_q := TUInt4096.Division(temp_fast, phi, check_fast);

  // The trusted reference version
  DivMod_Reference(temp_fast, phi, dummy_q, check_ref);

  WriteLn('Fast Check Result:      ', check_fast.ToString);
  WriteLn('Reference Check Result: ', check_ref.ToString);

  if (check_fast = one) and (check_ref = one) then
  begin
     WriteLn('CONCLUSION: Both checks passed. The error is somewhere else (highly unlikely).');
  end
  else if (check_fast = check_ref) then
  begin
     WriteLn('CONCLUSION: Both checks failed identically. The error is in the logic of ModInverse, not DivMod.');
  end
  else
  begin
     WriteLn('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
     WriteLn('!!! BUG FOUND: The results differ. The bug is in your fast DivMod.');
     WriteLn('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
  end;
end;

procedure FindFailingDivisionCase;
var
  p, q, n, phi, e, d, one: TUInt4096;
  KeyPair: TRSAKey;
  IsKeyValid: Boolean;
begin
  e := 65537;
  // Loop until we find a key pair that FAILS verification
  while True do
  begin
    WriteLn('Generating new p and q...');
    p := TUInt4096.MakePrime(512); // Use 512-bit primes to find the bug faster
    q := TUInt4096.MakePrime(512);
    if p = q then Continue;
    phi := TUInt4096.Multiplication(
      TUInt4096.Subtraction(p, TUInt4096.One),
      TUInt4096.Subtraction(q, TUInt4096.One)
    );
    if not (TUInt4096.GCD(e, phi) = TUInt4096.One) then Continue;
    // Calculate d using your normal ModInverse
    d := ModInverse(e, phi);
    // Verify the key
    IsKeyValid := VerifyKeyPair(d, e, phi);
    if not IsKeyValid then
    begin
      // We found a failure! Now we analyze it.
      WriteLn;
      WriteLn('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
      WriteLn('!!! FAILED KEY VERIFICATION. ANALYZING...');
      WriteLn('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
      WriteLn('Failing Phi: ', phi.ToString);
      WriteLn;

      // Now, run both versions of ModInverse and compare their quotients
      d := ModInverse_Fast_WithDebug(e, phi);
      d := ModInverse_Reference(e, phi);

      WriteLn;
      WriteLn('Compare the "Fast Q" and "Ref Q" lists above.');
      WriteLn('The first place they differ reveals the bug.');
      WriteLn('The inputs to DivMod that caused the difference are the');
      WriteLn('values of r and new_r from the *previous* successful iteration.');
      Break; // Exit the loop now that we have our analysis
    end
    else
    begin
      WriteLn('Key was valid. Trying again...');
    end;
  end;
end;

procedure Run;
  var n, n1, n2, r: TUInt4096;
  var i, j, k: Int32;
  var s: String;
  var Key: TRSAKey;
begin
  Randomize;
  //TestMultiplication;
  //TestVerificationFailure;
  //FindFailingDivisionCase;
  //Exit;
  Key := GenerateRSAKey(2048);
  n := EncryptStr('Hello World!', Key);
  WriteLn(n.ToString);
  s := DecryptStr(n, Key);
  WriteLn(s);
  ReadLn;
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

