program TestInt4096;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils;

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
  a := TUInt4096.One shl 32;
  b := 7;
  Actual := a mod b;
  WriteLn(IntPow(2, 2));
  Expected := IntPow(2, 64) mod 7;//4;
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

procedure TestPowMod_Simple;
var
  Base, Exponent, Modulus, Expected, Actual: TUInt4096;
begin
  WriteLn('--- Testing PowMod with 3^6 mod 7 ---');
  Base := 3;
  Exponent := 6;
  Modulus := 7;
  Expected := 1;
  Actual := TUInt4096.PowMod2(Base, Exponent, Modulus);
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

{
procedure TestMontMult_Simple;
var
  Context: TMontgomeryReduction.TContext;
  InputA, InputB, Expected, Actual: TUInt4096;
begin
  WriteLn('--- Testing MontMult with (3*5) mod 7 ---');
  Context := TMontgomeryReduction.InitContext(AssignInt(7));

  // Use the pre-calculated Montgomery form values
  InputA := AssignInt(5); // This is 3 in Montgomery form
  InputB := AssignInt(6); // This is 5 in Montgomery form
  Expected := AssignInt(4); // This is (3*5) in Montgomery form

  // Run your MontMult function
  Actual := TMontgomeryReduction.MontMult(Context, InputA, InputB);

  // Check the result
  if IsEqual(Actual, Expected) then
    WriteLn('SUCCESS: MontMult works for the simple test case!')
  else
    WriteLn('FAIL: MontMult returned the wrong result.');
end;
}

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

procedure Run;
  var n, n1, n2, r: TUInt4096;
begin
  Randomize;
  //TestShl;
  //TestPowMod_Simple;
  TestMod;
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
  n := TUInt4096.MakePrime(1024);
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

begin
  Run;
end.

