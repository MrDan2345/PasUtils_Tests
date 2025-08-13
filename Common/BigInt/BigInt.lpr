program BigInt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils,
  SysUtils;

procedure Run;
  var a0, b0, c0: TUInt4096;
  var a1, b1, c1: TUInt8192;
  var s0, s1: String;
  var i: Int32;
begin
  for i := 1 to 100 do
  begin
    a0 := TUInt4096.MakeRandom(2048);
    a1 := a0.ToString;
    b0 := TUInt4096.MakeRandom(1024);
    b1 := b0.ToString;
    c0 := a0 div b0;
    c1 := a1 div b1;
    s0 := c0.ToString;
    s1 := c1.ToString;
    if s0 <> s1 then
    begin
      WriteLn('Error!');
      Exit;
    end;
  end;
  WriteLn('Success');
end;

begin
  Run;
  ReadLn;
end.

