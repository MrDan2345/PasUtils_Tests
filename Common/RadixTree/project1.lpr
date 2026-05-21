program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

const TestData: array of String = (
  // Group 1: High overlap (The 'run' family)
    'run',
    'runner',
    'running',
    'runway',
    'runnable',
    'runoff',

    // Group 2: High overlap (The 'inter' family)
    'inter',
    'interface',
    'internal',
    'internet',
    'interval',
    'interact',
    'interstellar',

    // Group 3: Subtle overlap (Short prefixes)
    'cat',
    'category',
    'catalyst',
    'catapult',
    'caution',

    // Group 4: Near-misses (Different characters at the end)
    'test_1',
    'test_2',
    'test_3',
    'test_a',
    'test_b',

    // Group 5: Long strings with shared starts
    'supercalifragilistic',
    'supercalifragilisticexpialidocious',
    'supernatural',
    'superstructure',
    'superhero',

    // Group 6: Semi-random noise (Unique keys)
    'apple',
    'banana',
    'cherry',
    'date',
    'elderberry',
    'fig',
    'grape',
    'honeydew',
    'iceberg',
    'jackfruit',
    'kiwi',
    'lemon',
    'mango',
    'nectarine',
    'orange',
    'papaya',
    'quince',
    'raspberry',
    'strawberry',
    'tangerine',
    'ugli',
    'voavoe',
    'watermelon',
    'xigua',
    'youngberry',
    'zucchini',
    'alpha_beta',
    'gamma_delta',
    'epsilon_zeta',
    'theta_iota',
    'kappa_lambda',
    'mu_nu',
    'xi_omicron',
    'pi_rho'
);

procedure Run;
  var RadixTree: TURadixTree;
  var Data: specialize TUArray<String>;
  var i, j, n0, n1: Int32;
  var SearchStr: String;
begin
  Data.Add(TestData);
  for i := 0 to Data.LastIndex do
  begin
    RadixTree.Add(Data[i], i);
  end;
  RadixTree.Sort;
  RadixTree.Dump;
  Randomize;
  for i := 0 to 6 do
  begin
    n0 := Random(Data.Count);
    SearchStr := Data[n0];
    if Random(10) > 6 then
    begin
      if Random(2) = 0 then
      begin
        j := Random(Length(SearchStr) shr 1);
        if j = 0 then j := 1;
        Delete(SearchStr, Length(SearchStr) - j + 1, j);
      end
      else
      begin
        for j := 0 to Random(10) do
        begin
          SearchStr += Char(Ord('a') + Random(Ord('z') - Ord('a') + 1));
        end;
      end;
      n0 := -1;
    end;
    n1 := RadixTree.Find(SearchStr);
    if n0 > -1 then
    begin
      WriteLn('Searching[', n0, ']: ', Data[n0], '; Found[', n1, ']: ', Data[n1]);
    end
    else if n1 > -1 then
    begin
      WriteLn('Searching ', SearchStr, '; Found[', n1, ']: ', Data[n1]);
    end
    else
    begin
      WriteLn('Searching ', SearchStr, '; Not Found');
    end;
  end;
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

