# erlxlsx
SpreadsheetML (XLSX) generator library

### Usage Example
```erlang
f().
File = "xlsx_test/test.xlsx".
Sheet = #{1 => #{'A' => #{style => 1, data => "Test"}, 'B' => 1},
          2 => #{'A' => "Test1", 'B' => #{style => 0, data => 2}},
          3 => #{'A' => #{style => 2, data => "K2 Home", href => "http://www.k2informatics.ch"}}}.
Style = #{fonts => [#{name => "Calibri", color => "FF0000FF", size => 12.0,
                      bold => true, italics => true, underline => true, strike => true},
                    #{name => "Calibri", color => "FF0000FF", size => 12.0,
                      bold => false, italics => true, underline => false, strike => true},
                    #{name => "Calibri", color => "0563C1FF", size => 12.0,
                      bold => false, italics => false, underline => true, strike => false}],
          fills => [#{type => "solid", fg => "FFFF00FF", bg => "FFFF00FF"},
                    #{type => "solid", fg => "FFFFFF00", bg => "FFFFFF00"},
                    #{type => "solid", fg => "FFFFFFFF", bg => "FFFFFFFF"}],
          xfs => [#{fill => 0, font => 0},#{fill => 1, font => 1},#{font => 2}]}.

% with auto filter
{ok, FC} = erlxlsx:create(File, <<"Test">>, Sheet, Style, #{autoFilter => <<"A1:B1">>}).

% without auto filter
{ok, FC} = erlxlsx:create(File, <<"Test">>, Sheet, Style).

file:write_file(File, FC).
```