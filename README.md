# erlxlsx
SpreadsheetML (XLSX) generator library

### Usage Example
```erlang
l(xlsx_writer).
f(FC).
{ok, FC} = xlsx_writer:create_xlsx("xlsx_test/test.xlsx", <<"Test">>,
    #{1 => #{'A' => #{style => 1, data => "Test"}, 'B' => 1},
      2 => #{'A' => "Test1", 'B' => #{style => 0, data => 2}}},
    #{fonts => [#{name => "Calibri", color => "FF0000FF", size => 12.0, bold => true, italics => true, underline => true, strike => true},
                #{name => "Calibri", color => "FF0000FF", size => 12.0, bold => false, italics => true, underline => false, strike => true}],
      fills => [#{type => "solid", fg => "FFFF00FF", bg => "FFFF00FF"},#{type => "solid", fg => "FFFFFF00", bg => "FFFFFF00"}],
      xfs => [#{fill => 0, font => 0},#{fill => 1, font => 1}]}).
file:write_file("xlsx_test/test.xlsx", FC).
```
