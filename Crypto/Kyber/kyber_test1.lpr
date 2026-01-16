program kyber_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CryptoUtils;

class function TestNTT: Boolean;
var
  Original, Transformed, Recovered: TUKyber768.TPolynomial;
  i: Int32;
  AllMatch: Boolean;
begin
  WriteLn('Testing NTT/InvNTT...');
  for i := 0 to TUKyber768.N - 1 do
  begin
    Original[i] := i mod 17;
  end;
  Transformed := Original;
  TUKyber768.NTT(Transformed);

  WriteLn('After NTT, first 8 coefficients:');
  for i := 0 to 7 do
  begin
    Write(Transformed[i]:6, ' ');
  end;
  WriteLn;

  Recovered := Transformed;
  TUKyber768.InvNTT(Recovered);

  WriteLn('After InvNTT, first 8 coefficients:');
  for i := 0 to 7 do
  begin
    Write(Recovered[i]:6, ' ');
  end;
  WriteLn;

  WriteLn('Original, first 8 coefficients:');
  for i := 0 to 7 do
  begin
    Write(Original[i]:6, ' ');
  end;
  WriteLn;

  AllMatch := True;
  for i := 0 to TUKyber768.N - 1 do
  begin
    if Recovered[i] = Original[i] then Continue;
    if AllMatch then WriteLn('Mismatches found:');
    WriteLn(
      '  Index ', i:3, ': original=', Original[i]:5,
      ' recovered=', Recovered[i]:5
    );
    AllMatch := False;
    if i > 10 then Break;
  end;

  Result := AllMatch;

  if Result then
  begin
    WriteLn('✓ NTT/InvNTT test PASSED!')
  end
  else
  begin
    WriteLn('✗ NTT/InvNTT test FAILED!');
  end;
end;

procedure Run;
begin
  TestNTT;
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

