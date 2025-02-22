program BigInt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  SysUtils;

function IsPrime(n: Int64): Int64;
  var i: Int64;
begin
    if n <= 1 then Exit(1);
    if n <= 3 then Exit(0);
    if (n mod 2 = 0) then Exit(2);
    if (n mod 3 = 0) then Exit(3);
    i := Int64(5);
    while i * i <= n do
    begin
      if n mod i = 0 then Exit(i);
      if n mod (i + 2) = 0 then Exit(i + 2);
      Inc(i, 6);
    end;
    Exit(0);
end;

procedure Run;
  var n1, n2, n3, r: TUBigInt;
  //const Value = '-98765432109876543219876543210987654321';
  //const Value = '98765432121';
  //const Value = '987';
  var i: Int64;
  var pt, t: UInt64;
begin
  n1 := TUBigInt.Make('91872670824640586998765432109876543219876543210987654322');
  //n1 := TUBigInt.Make('98765432121');
  //n3 := TUBigInt.Division(n1, n2);
  //n2 := TUBigInt.Make('275923482734998765432109876543219876543210987654321');
  //n1 := TUBigInt.Make('$ffff');
  {n2 := TUBigInt.Make('43');
  pt := GetTickCount64;
  for i := 0 to 100000 - 1 do
  begin
    n3 := TUBigInt.Division(n1, n2);
  end;
  t := GetTickCount64;
  WriteLn(t - pt);
  }
  //n3 := TUBigInt.Sub(n1, n2);
  //n3 := TUBigInt.Multiplication(n1, n2);
  //n3 := TUBigInt.Division(n1, n2, r);
  //n := TUBigInt.Make(-9876);
  //n := TUBigInt.Make(9876);
  //WriteLn(n3.ToString);
  //WriteLn(n2.ToString);
  //WriteLn(n3.ToString, ' : ', r.ToString);
  //Randomize;
  Randomize;
  n1 := TUBigInt.MakePrime(4096);
  //i := n1.ToInt64;
  //WriteLn(n1.ToString, ' is prime = ', IsPrime(i));
  WriteLn(n1.ToString);
end;

begin
  Run;
  ReadLn;
end.

