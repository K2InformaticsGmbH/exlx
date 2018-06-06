-ifndef(_XLSX_HRL_).
-define(_XLSX_HRL_, true).

-define(XMLTAG, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>").

-define(FMTSCHEMA_URL_PREFIX, "http://schemas.openxmlformats.org").
-define(OXML_PKG, ?FMTSCHEMA_URL_PREFIX"/package/2006").
-define(OXML_SST, ?FMTSCHEMA_URL_PREFIX"/spreadsheetml/2006/main").
-define(OXML_ODR, ?FMTSCHEMA_URL_PREFIX"/officeDocument/2006/relationships").
-define(OXML_MRC, ?FMTSCHEMA_URL_PREFIX"/markup-compatibility/2006").

-define(OXML_CT, "application/vnd.openxmlformats").
-define(OXML_CT_REL, ?OXML_CT"-package.relationships+xml").
-define(OXML_CT_SSX, ?OXML_CT"-officedocument.spreadsheetml.sheet.main+xml").
-define(OXML_CT_WSX, ?OXML_CT"-officedocument.spreadsheetml.worksheet+xml").
-define(OXML_CT_STL, ?OXML_CT"-officedocument.spreadsheetml.styles+xml").

-define(CONTENT_TYPES_PATH, "[Content_Types].xml").
-define(CONTENT_TYPES_BIN,
        <<?XMLTAG
"<Types xmlns=\""?OXML_PKG"/content-types\">"
    "<Default Extension=\"rels\" ContentType=\""?OXML_CT_REL"\"/>"
    "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
    "<Override PartName=\"/workbook.xml\" ContentType=\""?OXML_CT_SSX"\"/>"
    "<Override PartName=\"/worksheet.xml\" ContentType=\""?OXML_CT_WSX"\"/>"
    "<Override PartName=\"/styles.xml\" ContentType=\""?OXML_CT_STL"\"/>"
"</Types>"
        >>).

-define(DOT_RELS_PATH, "_rels/.rels").
-define(DOT_RELS_BIN,
        <<?XMLTAG
"<Relationships xmlns=\""?OXML_PKG"/relationships\">"
    "<Relationship Id=\"rId1\" Type=\""?OXML_ODR"/officeDocument\" "
                  "Target=\"workbook.xml\"/>"
"</Relationships>"
        >>).

-define(WORKBOOK_PATH, "workbook.xml").
-define(WORKBOOK_BIN(__SheetName),
        <<?XMLTAG
"<workbook xmlns=\""?OXML_SST"\" xmlns:r=\""?OXML_ODR"\">"
    "<sheets><sheet name=\"",__SheetName/binary,"\" "
                   "sheetId=\"1\" r:id=\"rId1\"/></sheets>"
"</workbook>"
        >>).

-define(WORKBOOK_RELS_PATH, "_rels/workbook.xml.rels").
-define(WORKBOOK_RELS_BIN,
        <<?XMLTAG
"<Relationships xmlns=\""?OXML_PKG"/relationships\">"
    "<Relationship Id=\"rId1\" Type=\""?OXML_ODR"/worksheet\" "
                  "Target=\"worksheet.xml\"/>"
    "<Relationship Id=\"rId2\" Type=\""?OXML_ODR"/styles\" "
                  "Target=\"styles.xml\"/>"
"</Relationships>"
        >>).

-define(WORKSHEET_RELS_PATH, "_rels/worksheet.xml.rels").
-define(WORKSHEET_RELS_BIN(__Relationships),
        <<?XMLTAG
"<Relationships xmlns=\""?OXML_PKG"/relationships\">",
%        "<Relationship Id=\"rIdA3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink\" Target=\"http://www.k2informatics.ch/\" TargetMode=\"External\" />
    __Relationships/binary,
"</Relationships>"
        >>).

-define(WORKSHEET_PATH, "worksheet.xml").
-define(WORKSHEET_BIN(__SheetData,__AutoFilter),
        <<?XMLTAG
"<worksheet xmlns=\""?OXML_SST"\" xmlns:r=\""?OXML_ODR"\" xmlns:mc=\""?OXML_MRC"\">"
    "<sheetViews><sheetView workbookViewId=\"0\"/></sheetViews>"
    "<sheetFormatPr baseColWidth=\"10\" defaultColWidth=\"22.7109375\" defaultRowHeight=\"15\"/>"
    "<sheetData>",
        __SheetData/binary,
    "</sheetData>",
    __AutoFilter/binary,
"</worksheet>"
        >>).

-define(STYLES_PATH, "styles.xml").
-define(STYLES_BIN(__StyleData),
        <<?XMLTAG
"<styleSheet xmlns=\""?OXML_SST"\">",
    __StyleData/binary,
"</styleSheet>"
        >>).

-endif. %% _XLSX_HRL_
