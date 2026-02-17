program DelegateTest;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

type TMyRec = record
  procedure Test1(const Args: array of const);
  procedure Test2(const Args: array of const);
end;

procedure Test3(const Args: array of const);
begin
  WriteLn('Test3');
end;

procedure TMyRec.Test1(const Args: array of const);
begin
  WriteLn('Test1');
end;

procedure TMyRec.Test2(const Args: array of const);
begin
  WriteLn('Test2')
end;

procedure Run;
  var MyRec: TMyRec;
  var Delegate: TUDelegate;
begin
  MyRec := Default(TMyRec);
  Delegate.Add(@MyRec.Test1);
  Delegate.Add(@MyRec.Test2);
  Delegate.Add(@Test3);
  Delegate.Broadcast([]);
  Delegate.Remove(@Test3);
  Delegate.Remove(@MyRec.Test1);
  Delegate.Broadcast([]);
end;

begin
  Run;
{$if defined(windwos)}
  ReadLn;
{$endif}
end.

