unit neoTextutils;

{$mode delphi}

interface
   uses classes,stdctrls,SysUtils, INIfiles, Dialogs, windows, neotypedef;

// **********************************************************************
// Type definitions
// **********************************************************************
type
  TReplace = record
     FindText        : string;
     ReplaceText     : string;
     IgnoreCase      : Boolean;
     WholeWordsOnly  : Boolean;
     ReplaceAll      : Boolean;
  end;

  TNumKeepOpt = set of (optKeepDecimalPoint);

  TReplacePtr = ^TReplace;

  TReplaceList = class(TList)
     private
       LocFN : string;
     public
       function  GetItem(index : integer) : TReplace;
       procedure SetItem(Index : integer; x : TReplace);
       procedure AddItem(Find: string; Replace : string; Ignore : Boolean; Whole : Boolean; All:Boolean);
       procedure Delete(index : integer);
       procedure DeleteByHandle(x : Pointer);
       procedure Clear; override;
       procedure NewSection(FN : string; section : string);
       procedure StringsToReplaceList(x : Tstrings);
       function  ReplaceStr(index : integer) : string;

       function LoadFromFile(FN : string; section : string) : Boolean;
       function SaveToFile(FN : string; section : string) : Boolean;

       constructor Create(FN : string); reintroduce; overload;
  end;

// **********************************************************************
// Function declarations
// **********************************************************************
function HexToInt(const Str : String) : Integer;
function FindTextinList(Txt : string; x : TStrings) : integer;
function FindSubStrInList(Txt : string; x : TStrings) : integer;
function CheckHexCharsOnly(x : string) : Boolean;
function CheckIntegerString(x : string) : Boolean;
function CheckFloatString(x : string) : Boolean;
function IsAlphaNumerical(x : char) : Boolean;
function GetNumOccurencesOfChar(x : string; y:char): integer;
function IsAlpha(x : char) : Boolean;
function LastAlpha(x : string) : integer;
function IsNumerical(x : char) : Boolean;
function IsPrintable(x : char) : Boolean; overload;
function IsPrintable(x : byte) : Boolean; overload;

function CheckValidIdentifier(x : string) : Boolean;
function TrimFromLast(x : string; y : char) : string;
function TrimBefore(x : string; y : char) : string;
function TrimToChar(x : string; y : char) : string;
function TrimBothToChar(x : string; y : char) : string;
function ConstCharArray(Num : integer; x : char):string;
function FindSelectedItem(x : TListBox) : integer;
function GetNextWord(var x : string; y : char) : string; overload;
function GetNextWord(var x : string; y : char; DelChar : Boolean) : string; overload;
function GetNextWord(var x : string; y : char; QuoteCh : char) : string; overload;
function GetLastWord(var x : string; y : char) : string;

function TrimFirstCharLeft(x : string; y : char) : string;
function TrimCharLeft(x : string; y : char) : string;
function TrimCharRight(x : string; y : char) : string;
function TrimChar(x : string; y : char) : string;
function SimplifyStatement(x : string) : string;
function FindLast(x : char; y:string) : integer;
function FindChar(x : char; y : string) : integer;  overload;
function FindChar(x : char; y : string; startpos : integer) : integer; overload;
function BoolToStr(x : Boolean) : string; overload;
function BoolToStr(x : Boolean; TrueStr,FalseStr : string) : string; overload;
function StrToBool(x : string; CaseSensitive : Boolean) : boolean; overload;
function StrToBool(x : string; TrueStr,FalseStr : string;
                   CaseSensitive : Boolean) : Boolean; overload;
function MultiReplace(InStr : string; ReplaceList : TReplaceList) : string;
function ExtractArray(var x : string): string;
function KeepNumericalsOnly(x : string; Opt: TNumKeepOpt = []) : string;
function CharCount(x : string; y : char; p : integer) : integer;
function GetClipBoardColCount(x : string) : integer;
function FindMatchingBrace(x : string; OpenBrace, CloseBrace : char; startpos : integer) : integer;
function StripNonNumericals(x: string) : string;
function EndsWith(x,y : string) : Boolean; // check if string x ends with string y
function StartsWith(x,y : string) : Boolean; // check if string x ends with string y
function IntToStr(Value : integer; NumDigits : integer) : string; overload;
procedure SplitString(splitchar : char; TempStr : string; var TempStr1,TempStr2 : string); overload;
function SplitString(splitchar : char; str : string) : TStringList; overload;
function GetSubString(x : string; Startc,Endc : integer) : string;
function FindFirstNonIdentifierChar(x : string; startpos : integer) : integer;
function CountFirstChar(x : string) : integer;
function GetTextAfterChar(x : string; y : char) : string;
function IsCtrlChar(x : char) : Boolean;
function FindFirstCapitalChar(x : string) : char;
function IntToBin(x : integer; NumDigits : integer) : string;
function GetEnvVar(VarName : string; var RetStr: string) : Boolean;
function SetEnvVar(VarName : string; Value: string) : Boolean;
function Check2DigitVersion(x : string) : Boolean;
function Format2DigitVersion(x : string) : string;
function dateTimeToStrForFile(x : TDateTime) : string;
function ConvertBufToLine(CurAddr : integer; Buffer : PByteArray; BytesPerLine : integer) : string;
function FindFirstNumericorCapitalChar(x : string) : char;
function ParseHashCharCodes (S : string) : String;
procedure CheckControlMChar(var s : string);
procedure ParseSepStrToTStrings(S: string; sep : char; t : TStrings);
function RemoveStringFromList(x : string; y : TStrings) : boolean;
function CreateRSDPMenuTextWithShortcut(x : string) : string;
function Getproperty(x : string; marker : string) : string;

implementation
uses
   neoFileUtils;

// **********************************************************************
// Constructor for TreplaceList
// **********************************************************************
constructor TReplaceList.Create(FN : string);
begin
   inherited Create;

   LocFN := FN;
   LoadFromFile(FN,'Absorb');
end;

// **********************************************************************
// Get an item from the replace list
// **********************************************************************
function  TReplaceList.GetItem(index : integer) : TReplace;
var
   temp : TReplace;
begin
   if index >= count then
   begin
      with temp do
      begin
        FindText        := '<find>';
        ReplaceText     := '<replace>';
        IgnoreCase      := False;
        WholeWordsOnly  := True;
        ReplaceAll      := True;
      end
   end
   else
      temp := TReplacePtr(Items[index])^;

   GetItem := temp;
end;

// **********************************************************************
// Get the replace string
// **********************************************************************
function  TReplaceList.ReplaceStr(index : integer) : string;
var
   temp : TReplace;
begin
   temp   := GetItem(index);
   Result := temp.ReplaceText;
end;

// **********************************************************************
// Set an item in the replace list
// **********************************************************************
procedure TReplaceList.SetItem(Index : integer; x : TReplace);
begin
   if Index >= count then Exit;

   TreplacePtr(Items[index])^ := x;
end;

// **********************************************************************
// Add a replace item to the list
// **********************************************************************
procedure TReplaceList.AddItem(Find: string; Replace : string; Ignore : Boolean; Whole : Boolean; All:Boolean);
var
   NewItem : TReplacePtr;
begin
   New(NewItem);
   NewItem^.FindText       := Find;
   NewItem^.ReplaceText    := Replace;
   NewItem^.IgnoreCase     := Ignore;
   NewItem^.WholeWordsOnly := Whole;
   NewItem^.ReplaceAll     := All;

   Add(NewItem);
end;

// **********************************************************************
// Delete an item from the replace list
// **********************************************************************
procedure TReplaceList.Delete(index : integer);
begin
   if Index >= count then Exit;

   dispose(TReplacePtr(Items[index]));
   inherited Delete(index);;
end;

// **********************************************************************
// Delete an item by referring to the handle
// (will delete all references to this item!)
// **********************************************************************
procedure TReplaceList.DeleteByHandle(x : Pointer);
var
   i : integer;
begin
   if count = 0 then Exit;

   for i:=0 to count-1 do
   begin
     if Items[i] = x then
        delete(i);
   end;

end;

// **********************************************************************
// Clear the replace list
// **********************************************************************
procedure TReplaceList.Clear;
var
   i : integer;
begin
   if count = 0 then Exit;

   for i:=0 to count-1 do
      dispose(TreplacePtr(Items[i]));

   inherited Clear;
end;

// **********************************************************************
// Load replace options from a file
// **********************************************************************
function TReplaceList.LoadFromFile(FN : string; section : string) : boolean;
var
   ReplFile : TINIFile;
   temp     : TstringList;
begin
   ReplFile := TINIFile.Create(FN);
   temp := TStringList.Create;

   ReplFile.ReadSectionvalues(section,temp);
   StringsToReplaceList(temp);
   temp.Free;
   ReplFile.Free;

   LoadFromFile := true;
end;

// **********************************************************************
// Save replace options to a file
// **********************************************************************
function TReplaceList.SaveToFile(FN : string; Section : string) : Boolean;
var
   ReplFile : TINIFile;
   tempstr  : string;
   tempitem : Treplace;
   i        : integer;
begin
   SaveToFile := False;
   if FileExists(FN) then
      if not CreateBackup(FN,'.REPLBAK',bfAddExt,bmCopyFile) then
      begin
         MessageDlg('Could not create a backup of the replacement file.',mtWarning,
            [mbOK],0);
      end;
      
   if count = 0 then
   begin
      Exit;
   end;
   ReplFile := TINIFile.Create(FN);

   for i := 0 to count-1 do
   begin
      tempItem := getItem(i);
      TempStr := TempItem.FindText + #27 +
                 TempItem.ReplaceText + #27 +
                 BoolToStr(TempItem.Ignorecase) + #27 +
                 BoolToStr(TempItem.WholeWordsOnly) + #27 +
                 BoolToStr(TempItem.replaceAll) + #27;
      ReplFile.WriteString(Section,'Replace'+SysUtils.IntToStr(i),tempstr);
   end;

   ReplFile.Free;
   SaveToFile := True;
end;

// **********************************************************************
// Clear the section associated with the replace list
// **********************************************************************
procedure TReplaceList.NewSection(FN : string; section : string);
var
   ReplFile : TINIFile;
begin
   ReplFile := TINIFile.Create(FN);

   ReplFile.EraseSection(section);
   ReplFile.Free;
end;

// **********************************************************************
// Convert list of strings to a replacelist item
// **********************************************************************
procedure TReplaceList.StringsToReplaceList(x : Tstrings);
var
   i,j  : integer;
   temp : string;
   ReplaceItem : TReplacePtr;
begin
   Clear;

   if x.Count = 0 then Exit;

   for i:= 0 to x.count-1 do
   begin
      New(ReplaceItem);
      temp := x.strings[i];
      j := pos('=',temp);
      temp := copy(temp,j+1,length(temp)-j);
      {$ifdef FPC}
          ReplaceItem^.FindText        := GetNextWord(temp,#27);
          ReplaceItem^.ReplaceText     := GetNextWord(temp,#27);
          ReplaceItem^.IgnoreCase      := StrToBool(GetNextWord(temp,#27),False);
          ReplaceItem^.WholeWordsOnly  := StrToBool(GetNextWord(temp,#27),False);
          ReplaceItem^.ReplaceAll      := StrToBool(GetNextWord(temp,#27),False);
      {$else}
          ReplaceItem.FindText        := GetNextWord(temp,#27);
          ReplaceItem.ReplaceText     := GetNextWord(temp,#27);
          ReplaceItem.IgnoreCase      := StrToBool(GetNextWord(temp,#27),False);
          ReplaceItem.WholeWordsOnly  := StrToBool(GetNextWord(temp,#27),False);
          ReplaceItem.ReplaceAll      := StrToBool(GetNextWord(temp,#27),False);
      {$endif}
      Add(ReplaceItem);
   end;

end;

// **********************************************************************
// Check if a string is a hex number
// **********************************************************************
function CheckHexCharsOnly(x : string) : Boolean;
var
  i      : integer;
  res    : Boolean;
begin
  CheckHexCharsOnly := False;
  if Length(x) < 1 then exit;

  res := True;
  for i:= 1 to length(x) do
     if not( ((x[i] >= '0') and (x[i] <= '9')) or ((x[i] >= 'A') and (x[i] <= 'F')) or
            ((x[i] >= 'a') and (x[i] <= 'f'))) then
            res := false;
  CheckHexCharsOnly := Res;
end;

// **********************************************************************
// Check if a charcter is alpha
// **********************************************************************
function IsAlpha(x : char) : Boolean;
var
   temp : char;
begin
   temp := UpCase(x);

   if ((temp >= 'A') and (temp <= 'Z')) then
         IsAlpha := True
   else
         IsAlpha := False;
end;

// **********************************************************************
// Check if a charcter is printable
// **********************************************************************
function IsPrintable(x : char) : Boolean;
var
    y : byte;
begin
    y := byte(x);
    Result := false;

    if (y >= 32) and (y <=127) then Result := true;
end;

// **********************************************************************
// Check if a charcter is printable
// All ASCII + TAB is printable
// **********************************************************************
function IsPrintable(x : byte) : Boolean; overload;
begin
    Result := false;

    if ((x >= 32) and (x <=127)) or (x = 9) then Result := true;
end;

// **********************************************************************
// Check if a charcter is numerical
// **********************************************************************
function IsNumerical(x : char) : Boolean;
var
   temp : char;
begin
   temp := UpCase(x);

   if ((temp >= '0') and (temp <= '9')) then
         IsNumerical := True
   else
         IsNumerical := False;
end;

// **********************************************************************
// Check if a charcter is alpha numerical
// **********************************************************************
function IsAlphaNumerical(x : char) : Boolean;
var
   temp : char;
begin
   temp := UpCase(x);

   if ((temp >= 'A') and (temp <= 'Z')) or
      ((temp >= '0') and (temp <= '9')) then
         IsAlphaNumerical := True
   else
         IsAlphaNumerical := False;
end;

// **********************************************************************
// Count the number of times a character
// **********************************************************************
function GetNumOccurencesOfChar(x : string; y:char) : integer;
var
   i,j : integer;
begin
   j := 0;
   for i := 1 to length(x) do
      if x[i] = y then inc(j);
   GetNumOccurencesOfChar := j;
end;

// **********************************************************************
// Find the last alpha character in a string
// **********************************************************************
function LastAlpha(x : string) : integer;
var
   i : integer;
begin
   LastAlpha := -1;
   for i := Length(x) downto 1 do
     if IsAlpha(x[i]) then
     begin
        LastAlpha := i;
        Exit;
     end;
end;

// **********************************************************************
// Hex text to integer conversion
// **********************************************************************
function HexToInt(const Str : String) : Integer;
var
   i    : integer;
   Ans  : integer;
begin
   Ans := 0;
   for i := 1 to length(Str) do
   begin
     if Str[i] = ' ' then break; 
     Ans := Ans shl 4;
     if ((Str[i] >= '0') and (Str[i] <= '9')) then
        Ans := Ans + ord(Str[i]) - ord('0')
     else if ((Str[i] >= 'a') and (Str[i] <= 'f')) then
        Ans := Ans + ord(Str[i]) - ord('a') + 10
     else if ((Str[i] >= 'A') and (Str[i] <= 'F')) then
        Ans := Ans + ord(Str[i]) - ord('A') + 10;
   end;
   HexToInt := Ans;
end;

// **********************************************************************
// Find a text string in a TString object
// **********************************************************************
function FindTextInList(Txt : string; x : TStrings) : integer;
var
   i      : integer;
   Ans    : integer;
begin
   Ans := -1;
   if x.count = 0 then
   begin
     FindTextInList := -1;
     Exit;
   end;

   for i:= 0 to x.count-1 do
      if x.strings[i] = Txt then
        if Ans < 0  then Ans := i;

   FindTextInList := Ans;
end;

// **********************************************************************
// Find a sub string in a TString object
// **********************************************************************
function FindSubStrInList(Txt : string; x : TStrings) : integer;
var
   i      : integer;
   Ans    : integer;
begin
   Ans := -1;
   if x.count = 0 then
   begin
     FindSubStrInList := -1;
     Exit;
   end;

   for i:= 0 to x.count-1 do
      if pos(Txt,x.strings[i])<>0 then
        if Ans < 0  then Ans := i;

   FindSubStrInList := Ans;
end;

// **********************************************************************
// Check valid identifier
// **********************************************************************
function CheckValidIdentifier(x : string) : Boolean;
var
   i : integer;
begin
   CheckValidIdentifier := True;
   for i := 1 to length(x) do
      if (not IsAlphaNumerical(x[i])) and (x[i] <> '_') then
         CheckValidIdentifier := False;
end;

// **********************************************************************
// Trim all text from a specific character to the end
// **********************************************************************
function TrimFromLast(x : string; y : char) : string;
var
   i,j : integer;
begin
   j := length(x);
   for i := 1 to length(x) do
     if x[i] = y then
        j := i;

   TrimFromLast := Copy(x,1,j-1);
end;

// **********************************************************************
// Constant character array
// **********************************************************************
function ConstCharArray(Num : integer; x : char) : string;
var
   temp : string;
   i    : integer;
begin
   temp := '';

   if Num < 1 then Exit;
   for i := 1 to Num do
      temp := temp + x;

   ConstCharArray := temp;
end;

// **********************************************************************
// Find the selected item in the listbox
// **********************************************************************
function FindSelectedItem(x : TListBox) : integer;
var
   i : integer;
begin
   FindSelectedItem := -1;
   if x.Items.Count = 0 then
      Exit;

   for i:= 0 to x.Items.Count-1 do
   begin
      if x.Selected[i] then FindSelectedItem := i;
   end;
end;

// **********************************************************************
// Get the last word in a string with seperator character
// **********************************************************************
function GetLastWord(var x : string; y : char) : string;
var
   i : integer;
begin
   i := FindLast(y,x);
   GetLastWord := Copy(x,i,length(x)-i+1);
   x := Copy(x,1,i-1);
end;

// **********************************************************************
// Get the next word in a string with seperator character
// **********************************************************************
function GetNextWord(var x : string; y : char) : string;
begin
   GetNextWord := GetNextWord(x,y,true);
end;

// **********************************************************************
// Get the next word in a string with seperator character
// **********************************************************************
function GetNextWord(var x : string; y : char; DelChar : Boolean) : string;
var
   temp  : string;
   i   : integer;
begin
   temp := x;
   if DelChar then
       temp := trimCharLeft(temp,y);

   i    := pos(y,temp);
   GetNextWord := Copy(temp,1,i-1);
   x := Copy(Temp,i,length(temp)-i+1);
end;

// **********************************************************************
// Get the next word in a string with seperator character,
// where the field
// **********************************************************************
function GetNextWord(var x : string; y : char; QuoteCh : char) : string; overload;
var
   temp    : string;
   i,j     : integer;
begin
   temp := x;
   temp := trimCharLeft(Temp,y);
   j    := pos(QuoteCh,temp);
   if j = 0 then
   begin
      GetNextWord := '';
      Exit;
   end;

   j    := FindChar(QuoteCh,temp,j+1);
   if j=0 then
   begin
      GetNextWord := '';
      Exit;
   end;

   i           := FindChar(y,temp,j+1);
   GetNextWord := Copy(temp,1,i-1);
   x           := Copy(Temp,i,length(temp)-i+1);
end;

// **********************************************************************
// Trim a only the first specific char to the left
// **********************************************************************
function TrimFirstCharLeft(x : string; y : char) : string;
var
   temp  : string;
begin
   temp := x;
   temp := trim(temp);
   if temp[1] = y then
      temp := copy(temp,2,length(temp)-1);

   TrimFirstCharLeft := temp;
end;

// **********************************************************************
// Trim a string of a specific type of characters from the left
// **********************************************************************
function TrimCharLeft(x : string; y : char) : string;
var
   temp : string;
begin
   if length(x) < 1 then Exit;
   temp := x;
   while (length(temp) > 1) and (temp[1] = y) do
      temp := copy(temp,2,length(temp)-1);
   TrimCharLeft := temp;
end;

// **********************************************************************
// Trim a string of a specific type of characters from the right
// **********************************************************************
function TrimCharRight(x : string; y : char) : string;
var
   temp : string;
begin
   if length(x) < 1 then Exit;
   temp := x;
   while (length(temp) > 1) and (temp[length(temp)] = y) do
      temp := copy(temp,1,length(temp)-1);
   TrimCharRight := temp;
end;

// **********************************************************************
// Trim a string of a specific type of characters
// **********************************************************************
function TrimChar(x : string; y : char) : string;
var
   temp : string;
begin
   Temp     := TrimCharLeft(x,y);
   Temp     := TrimCharRight(temp,y);
   TrimChar := Temp;
end;

// **********************************************************************
// Simplify a statement - ';' is assumed to indicate the end of the statement
// **********************************************************************
function SimplifyStatement(x : string) : string;
var
  EndOfStatement  : integer;
  WhiteSpaceStart : integer;
  WhiteSpaceEnd   : integer;

  i          : integer;
  temp       : string;
  temp2      : string;
begin
  EndOfStatement := FindLast(';',x);
  temp := copy(x,1,EndOfStatement-1);

  if (length(temp) > 2000) then
  begin
     i := pos('=',temp);
     temp := copy(temp,1,i-1)+';';
  end;

  temp := StringReplace(temp,'}',' } ',[rfReplaceAll]);
  temp := StringReplace(temp,'{',' { ',[rfReplaceAll]);
  temp := trim(temp);

  WhiteSpaceStart := -1;
  WhiteSpaceEnd   := -1;


  temp := StringReplace(temp,#$D,'',[rfReplaceAll]);
  temp := StringReplace(temp,#$A,'',[rfReplaceAll]);
//  temp := StringReplace(temp,#$9,'',[rfReplaceAll]);

  i := 1;
  while i < length(temp) do
  begin
     if (temp[i] = ' ') or (temp[i] = CHR_LF) or (temp[i] = CHR_RETURN) or
        (temp[i] = CHR_TAB) then
     begin
       if WhiteSpaceStart = (-1) then
       begin
          WhiteSpaceStart := i;
          WhiteSpaceEnd := i;
       end
       else
          WhiteSpaceEnd := i;
     end
     else
     begin
       if WhiteSpaceStart <> (-1) then
       begin
          temp2 := copy(temp,WhiteSpaceStart,WhiteSpaceEnd-WhiteSpaceStart+1);
          temp := StringReplace(temp,temp2,' ',[rfReplaceAll]);
          WhiteSpaceStart := -1;
          WhiteSpaceEnd   := -1;
       end;
     end;
     inc(i);
  end;
  SimplifyStatement := temp;
end;

// **********************************************************************
// Find a the last occurence of a character in a string
// **********************************************************************
function FindLast(x : char; y:string) : integer;
var
   i,j : integer;
begin
   j := 0;
   for i:= 1 to length(y) do
      if y[i] = x then j := i;
   FindLast := j;
end;

// **********************************************************************
// Find the first occurence of a character
// **********************************************************************
function FindChar(x : char; y : string) : integer;
begin
   FindChar := FindChar(x,y,1);
end;

// **********************************************************************
// Find the first occurence of a character after a certain position in a string
// **********************************************************************
function FindChar(x : char; y : string; startpos : integer) : integer; overload;
var
   i : integer;
begin
   for i:= startPos to length(y) do
      if y[i] = x then
      begin
         FindChar := i;
         Exit;
      end;
   FindChar := 0;
end;

// **********************************************************************
// Replace a string with multiple find/replace pairs
// **********************************************************************
function MultiReplace(InStr : string; ReplaceList : TReplaceList) : string;
var
   i,n     : integer;
   temp    : TReplace;
   ReplOpt : TReplaceFlags;
   res     : string;
begin
   Res := Instr;

   for i:=0 to ReplaceList.count-1 do
   begin
      temp := ReplaceList.GetItem(i);
      if temp.IgnoreCase then
         ReplOpt := [rfIgnoreCase]
      else
         ReplOpt := [];

      if temp.ReplaceAll then
         replOpt := ReplOpt + [rfReplaceAll];

      if temp.WholeWordsOnly then
      begin
         n := pos(temp.FindText,res);
         if (Res[n-1] = ' ') and (Res[n+length(temp.findText)] = ' ') then
             res := StringReplace(res,temp.findText,temp.replaceText,ReplOpt)
      end
      else
         res := StringReplace(res,temp.findText,temp.replaceText,ReplOpt);
   end;

   MultiReplace := Res;
end;

// **********************************************************************
// Replace a string with multiple find/replace pairs
// **********************************************************************
function BoolToStr(x : Boolean) : string;
begin
   BoolToStr := BoolToStr(x,'True','False');
end;

// **********************************************************************
// Replace a string with multiple find/replace pairs
// **********************************************************************
function BoolToStr(x : Boolean; TrueStr,FalseStr : string) : string; overload;
begin
   if x then
      BoolToStr := TrueStr
   else
      BoolToStr := FalseStr;
end;

// **********************************************************************
// Replace a string with multiple find/replace pairs
// **********************************************************************
function StrToBool(x : string; CaseSensitive : Boolean) : boolean; overload;
begin
   StrToBool := StrToBool(x,'True','False',CaseSensitive);
end;

// **********************************************************************
// Replace a string with multiple find/replace pairs
// **********************************************************************
function StrToBool(x : string; TrueStr,FalseStr : string;
                   CaseSensitive : Boolean) : Boolean; overload;
var
   temp_x         : string;
   temp_TrueStr   : string;
   temp_FalseStr  : string;
begin
   if not CaseSensitive then
   begin
      temp_x         := Uppercase(x);
      temp_TrueStr   := Uppercase(TrueStr);
      temp_FalseStr  := Uppercase(FalseStr);
   end
   else
   begin
      temp_x         := x;
      temp_TrueStr   := TrueStr;
      temp_FalseStr  := FalseStr;
   end;

   StrToBool := false;

   if temp_x = temp_TrueStr then
      StrToBool := True
   else if (temp_x = temp_FalseStr) then
      StrToBool := False;
end;

// **********************************************************************
// Remove all text before a certain character, excluding the character
// **********************************************************************
function TrimBefore(x : string; y : char) : string;
var
   n   : integer;
   res : string;
begin
   n := pos(y,x);
   res := copy(x,n,length(x)-n+1);
   TrimBefore := res;
end;

// **********************************************************************
// Remove all text before a certain character, including the character
// **********************************************************************
function TrimToChar(x : string; y : char) : string;
var
   temp : string;
begin
   temp := TrimBefore(x,y);
   temp := copy(temp,2,length(temp)-1);
   TrimToChar := temp;
end;

// **********************************************************************
// Extract the array info from a variable decl.
// **********************************************************************
function TrimBothToChar(x : string; y : char) : string;
var
  TempStr : string;
begin
  TempStr := TrimToChar(x,y);
  TempStr := TrimFromLast(TempStr,y);
  TrimBothToChar := TempStr;
end;

// **********************************************************************
// Extract the array info from a variable decl.
// **********************************************************************
function ExtractArray(var x : string): string;
var
   i,j : integer;
   temp : string;
begin
   i := pos('[',x);
   j := pos(']',x);

   temp := copy(x,i+1,j-i-1);
   x := stringreplace(x,temp,'',[]);
   i := pos('[',x);
   if i <> 0 then x[i] := ' ';
   j := pos(']',x);
   if j <> 0 then x[j] := ' ';
   ExtractArray := temp;
end;

// **********************************************************************
// Keep the numerical characters only
// **********************************************************************
function KeepNumericalsOnly(x : string; Opt: TNumKeepOpt = []) : string;
var
   res : string;
   i   : integer;
begin
   res := '';

   for i:= 1 to length(x) do
      if OptKeepDecimalPoint in Opt then
      begin
         if IsNumerical(x[i]) or (x[i]='.') then res := Res + x[i];
      end
      else
      begin
         if IsNumerical(x[i]) then res := Res + x[i];
      end;
   KeepNumericalsOnly := Res;
end;

// **********************************************************************
// Count the number of consective characters of a kind from a position
// in a string
// **********************************************************************
function CharCount(x : string; y : char; p : integer) : integer;
var
   i,j : integer;
begin
   i := 0;

   if (p < 1) or (p > length(x)) then
   begin
      CharCount := i;
      Exit;
   end;

   for j := p to length(x) do
   begin
      if x[j] <> y then
      begin
         CharCount := i;
         Exit;
      end
      else
         inc(i);
   end;

   CharCount := i;
end;

// **********************************************************************
// Return the number of columns of the text on the clipboard (for spreadsheets)
// **********************************************************************
function GetClipBoardColCount(x : string) : integer;
var
   tempstr : string;
   i       : integer;
begin
   i := pos(CHR_CR + CHR_LF,x);

   tempstr := copy(x,1,i-1);
   GetClipBoardColCount := GetNumOccurencesOfChar(TempStr,CHR_TAB) + 1;
end;

// **********************************************************************
// Find the number of times a substring appears in a string
// **********************************************************************
function GetNumSubStringOccurrences(x,y : string) : integer;
var
   tempstr : string;
   i,j     : integer;
begin
   tempstr := x;
   i := pos(y,tempstr);
   j := 1;
   while (i <> 0) do
   begin
      inc(j);
      tempstr := copy(tempstr,i,length(tempstr) - i + 1);
      i := pos(y,tempstr);
   end;
   GetNumSubStringOccurrences := j;
end;

// **********************************************************************
// Return the number of rows of the text on the clipboard (for spreadsheets)
// **********************************************************************
function GetClipBoardRowCount(x : string) : integer;
begin
   GetClipBoardRowCount := GetNumSubStringOccurrences(x,CHR_CR + CHR_LF);
end;

// **********************************************************************
// Check that a string forms an integer
// **********************************************************************
function CheckIntegerString(x : string) : Boolean;
var
   temp      : string;
   IsInteger : Boolean;
   i         : integer;
begin
   temp      := x;
   Temp      := trim(temp);
   if length(x) > 0 then
        IsInteger := True
   else
        IsInteger := False;

   for i := 1 to length(temp) do
      if not IsNumerical(temp[i]) then
          IsInteger := False;

   CheckIntegerString := IsInteger;
end;

// **********************************************************************
// Check if a string is a valid floating point number
// **********************************************************************
function CheckFloatString(x : string) : Boolean;
begin
   try
     StrToFloat(x);
     CheckFloatString := True;
   except
     CheckFloatString := False;
   end;
end;

// **********************************************************************
// Find a mathcing brace
// **********************************************************************
function FindMatchingBrace(x : string; OpenBrace, CloseBrace : char; startpos : integer) : integer;
var
   Depth : integer;
   i     : integer;
begin
   FindMatchingBrace := 0;
   i     :=FindChar(OpenBrace,x,StartPos);
   Depth := 0;

   while (i <= length(x))  do
   begin
           if x[i] = OpenBrace  then inc(Depth)
      else if x[i] = CloseBrace then
      begin
         dec(Depth);
         if Depth = 0 then
         begin
            FindMatchingBrace := i;
            Exit;
         end;
      end;
      inc(i);
   end;
end;

// **********************************************************************
// Remove all non numerical characters
// **********************************************************************
function StripNonNumericals(x: string) : string;
var
   temp : string;
   i    : integer;
begin
   temp := '';

   for i := 1 to length(x) do
      if IsNumerical(x[i]) then Temp := temp + x[i];

   StripNonNumericals := temp;
end;

// **********************************************************************
// Check if one string ends with another
// **********************************************************************
function EndsWith(x,y : string) : Boolean; // check if string x ends with string y
var
   temp : integer;
begin
   EndSwith := False;
   temp := pos(y,x);
   if temp <> 0 then
   begin
      if (length(x) - length(y)) = (temp-1) then
      begin
         EndsWith := True;
      end;
   end;
end;

// **********************************************************************
// See if a strings starts with a certain substring
// **********************************************************************
function StartsWith(x,y : string) : Boolean; // check if string x starts with string y
var
   temp : integer;
begin
   StartsWith := False;
   temp := pos(y,x);
   if temp = 1 then
   begin
      StartsWith := True;
   end;
end;

// **********************************************************************
// Int to string with fixed field length
// **********************************************************************
function IntToStr(Value : integer; NumDigits : integer) : string; overload;
var
   Temp : string;
begin
   Temp := SysUtils.IntToStr(Value);
   Temp := ConstCharArray(NumDigits-length(Temp),'0') + Temp;
   IntToStr := Temp;
end;

// **********************************************************************
// Split a string into 2 strings based on a character
// **********************************************************************
procedure SplitString(splitchar : char; TempStr : string; var TempStr1,TempStr2 : string);
var
   temp : integer;
begin
   temp := pos(SplitChar,TempStr);
   if Temp <> 0 then
   begin
      TempStr1 := copy(TempStr,1,temp-1);
      TempStr2 := copy(TempStr,temp+1,length(TempStr)-temp);
   end
   else
   begin
      TempStr1 := tempStr;
      TempStr2 := '';
   end;
end;

// **********************************************************************
// Split a string and return the TStrings list for the split up strings
// **********************************************************************
function SplitString(splitchar : char; str : string) : TStringList;
var
   	r : TStringList;
    t1,t2 : string;
    temp : integer;
    done : boolean;
begin
	r := TStringList.Create;
    done := false;
	t2 := str;

    while not done do
    begin
        temp := pos(SplitChar,t2);
        if Temp <> 0 then
   		begin
      		t1 := copy(t2,1,temp-1);
      		t2 := copy(t2,temp+1,length(t2)-temp);

            r.Add(t1);
   		end
   		else
   		begin
            r.Add(t2);
            done := true;
   		end;
	end;
    result := r;
end;

// **********************************************************************
// return the string from startc to endc
// **********************************************************************
function GetSubString(x : string; Startc,Endc : integer) : string;
begin
   Result := copy(x,Startc,Endc-Startc+1);
end;

// **********************************************************************
// return the string from startc to endc
// **********************************************************************
function FindFirstNonIdentifierChar(x : string; startpos : integer) : integer;
var
   i : integer;
begin
   Result := 0;
   for i := startpos to length(x) do
   begin
      if (not IsAlphaNumerical(x[i])) and (x[i] <> '_') then
      begin
         Result := i;
         Exit;
      end;
   end;
end;

// **********************************************************************
// Count the number of the first character that is identical
// **********************************************************************
function CountFirstChar(x : string) : integer;
var
   i  : integer;
   bc : char;
begin
   Result := 1;
   if Length(x) < 2 then Exit;

   i := 2;
   bc := x[1];
   while (i <= length(x)) do
      if x[i] = bc then
         inc(i)
      else
      begin
         Result := i-1;
         Exit;
      end;

   result := i-1;
end;

// **********************************************************************
// return all the text after the last occurence of a character
// **********************************************************************
function GetTextAfterChar(x : string; y : char) : string;
var
  Temp : integer;
begin
  Temp   := FindLast(y,x);
  Result := GetSubString(x,Temp+1,length(x));
end;

// **********************************************************************
// Check if a character is a control character
// **********************************************************************
function IsCtrlChar(x : char) : Boolean;
begin
   Result := False;
   if (ord(x) < 32)  or (ord(x) >= 128) then
   begin
      Result := True;
   end;
end;

// **********************************************************************
// Find the numeric char or first capital character in a string
// **********************************************************************
function FindFirstNumericorCapitalChar(x : string) : char;
var
    i : integer;
begin
    Result := #0;
    for i := 1 to length(x) do
        if ((x[i] >= 'A') and (x[i] <= 'Z')) or
           ((x[i] >= '0') and (x[i] <= '9')) then
        begin
            Result := x[i];
            Exit;
        end;
end;

// **********************************************************************
// Find the first capital character in a string
// **********************************************************************
function FindFirstCapitalChar(x : string) : char;
var
   i : integer;
begin
   Result := #0;
   for i := 1 to length(x) do
      if (x[i] >= 'A') and (x[i] <= 'Z') then
      begin
          Result := x[i];
          Exit;
      end;
end;

// **********************************************************************
// Convert an integer to a binary string
// **********************************************************************
function IntToBin(x : integer; NumDigits : integer) : string;
var
   i : integer;
begin
   Result := '';

   for i := NumDigits-1 Downto 0 do
   begin
      if (x and (1 shl i)) <> 0 then result := result + '1'
                                else result := result + '0';
   end;
end;

// **********************************************************************
// Get an enviroment string
// **********************************************************************
function GetEnvVar(VarName : string; var RetStr : string) : Boolean;
const
   BUF_SIZE = 4096;
var
   Buf  : array [0..BUF_SIZE-1] of char;
   temp : integer;
   i    : integer;
begin
   temp := GetEnvironmentVariable(pChar(VarName),@Buf[0],sizeof(Buf));

        if Temp = 0 then Result := False
   else if Temp > BUF_SIZE then Result := False
   else
   begin
      Result := True;
      RetStr := '';
      for i := 0 to Temp-1 do
      begin
         RetStr := RetStr + Buf[i];
      end;
   end;
end;

// **********************************************************************
// Set an enviroment string
// **********************************************************************
function SetEnvVar(VarName : string; Value: string) : Boolean;
begin
   Result := SetEnvironmentVariable(Pchar(VarName),PChar(Value));
end;

// **********************************************************************
// Check that this is a valid version string (in the form Vxx.yy)
// check is not case sensitive
// **********************************************************************
function Check2DigitVersion(x : string) : Boolean;
var
   vpos   : integer;
   temp   : string;
   dotpos : integer;

   minorstr : string;
   majorstr : string;
begin
   temp   := UpperCase(x);
   vpos   := pos('V',temp);
   dotpos := pos('.',temp);

   minorStr := copy(temp,dotpos+1,length(temp)-dotpos);
   majorStr := KeepNumericalsOnly(GetSubString(temp,vpos+1,dotpos-1));

   if CheckIntegerString(minorStr) and
      CheckIntegerString(MajorStr) then Result := true
                                   else Result := False;
end;

// **********************************************************************
// Format the 2 digit version string to a known format
// **********************************************************************
function Format2DigitVersion(x : string) : string;
var
   vpos   : integer;
   temp   : string;
   dotpos : integer;

   minorstr : string;
   majorstr : string;
begin
   temp   := UpperCase(x);
   vpos   := pos('V',temp);
   dotpos := pos('.',temp);

   minorStr := copy(temp,dotpos+1,length(temp)-dotpos);
   majorStr := KeepNumericalsOnly(GetSubString(temp,vpos+1,dotpos-1));

   Result := 'V '+trim(MajorStr) + '.' + trim(MinorStr);
end;

// **********************************************************************
// Format a TdateTime to use with a file
// **********************************************************************
function dateTimeToStrForFile(x : TDateTime) : string;
var
   Temp : string;
begin
   temp := dateTimeToStr(x);
   Temp := StringReplace(Temp,'/','_',[rfIgnoreCase, rfReplaceAll]);
   Temp := StringReplace(Temp,':','_',[rfIgnoreCase, rfReplaceAll]);

   Result := Temp;
end;

(*
/* ************************************************************************* */
/* <Function>   : ConvertLineToBuf                                           */
/* <Input>      : AnsiString Str   = string to convert to buffer             */
/*                int BytesPerLine = bytes on this stringline                */
/*                int AddrOfs      = offset of address in text to buffer     */
/*                BYTE *bBuf       = pointer to buffer                       */
/*                                                                           */
/* <Effect>     : converts a text line of hex entries to a buffer            */
/* <Return>     : None                                                       */
/* ************************************************************************* */*)
procedure ConvertLineToBuf(Str: string; BytesPerLine : integer;
                           AddrOfs : integer; bBuf : PByteArray);
var
    TempColonPos : integer;
    AddrStr      : string;
    Addr         : integer;
    TempStr      : string;
    j            : integer;
    TempSpacePos : integer;
    HexVal       : string;
begin
   TempColonPos := Pos(Str,':');

   if (TempColonPos = 0) then exit;

   AddrStr := Copy(Str,1,TempColonPos-1);
   Addr    := HexToInt(AddrStr) - AddrOfs;

   TempStr := Copy(Str,TempColonPos+1,length(Str)-TempColonPos-1);

   j := round(BytesPerLine);

   while(j <> 0) do
   begin
      TempStr := Trim(TempStr) + ' ';
      TempSpacePos := Pos(TempStr,' ');

      if (TempSpacePos <> 0) then
      begin
         HexVal := Copy(TempStr,1,TempSpacePos-1);
         bBuf[Addr] := HexToInt(HexVal);
      end;

      TempStr := Copy(TempStr,TempSpacePos+1,Length(TempStr)-TempSpacePos);
      dec(j);
      inc(Addr);
   end;
end;

(*
/* ************************************************************************* */
/* <Function>   : ConverBufToLine                                            */
/* <Input>      : AnsiString Str   = string to convert to buffer             */
/*                int BytesPerLine = bytes on this stringline                */
/*                int AddrOfs      = offset of address in text to buffer     */
/*                BYTE *bBuf       = pointer to buffer                       */
/*                                                                           */
/* <Effect>     : converts a text line of hex entries to a buffer            */
/* <Return>     : None                                                       */
/* ************************************************************************* */*)
function ConvertBufToLine(CurAddr : integer; Buffer : PByteArray; BytesPerLine : integer) : string;
var
    TempStr : string;
    TextStr : string;
    j       : integer;
begin
    TempStr := IntToHex(CurAddr,8) + ': ';
    TextStr := '';

    for j := 0 to BytesPerLine-1 do
    begin
       TempStr := TempStr + IntToHex(Buffer[j],2) + ' ';
       if (Buffer[j] = 0) then
          TextStr := TextStr + #1
       else
          TextStr := TextStr + chr(Buffer[j]);
    end;

    Result := Trim(TempStr + ' ' + TextStr);
end;

// *************************************************************************
// convert text representation of esc char to a char
// *************************************************************************
function ParseHashCharCodes (S : string) : String;
var
    R : string;
begin
    R := S;

    R := stringreplace(R,'#27',#27,[rfReplaceAll]);

    Result := R;
end;

// *************************************************************************
// parse a text string fpr printing on a textmemo
// for correct handling of ^m (13) character
// *************************************************************************
procedure CheckControlMChar(var s : string);
var
    Res : string;
    i   : integer;
    j,k : integer;
    f   : boolean;
    s1,s2 : string;
begin
    Res := s;

    i := length(res)-1;
    while (i > 2) do
    begin
        if (res[i] = #8) then
        begin
            s1 := Copy(res,1,i-2);
            s2 := Copy(res,i+1,length(res)-1);
            res := s1 + s2;
        end;
        if (res[i] = #13) and
           (IsPrintable(res[i+1])) then
        begin
            // we have ^M
            j := i-1;
            f := false;
            while (j > 2) and (not f) do
            begin
                if not IsPrintable(res[j]) then
                begin
                     k := j;
                     f := true;
                end;
                j := j - 1;
            end;
            // now i = pos of ^M
            // k = pos to delete to
            s1 := Copy(res,1,k);
            s2 := Copy(res,i+1,length(res)-i-1);
            res := s1 + s2;
        end;
        i := i - 1;
    end;
    s := Res;
end;

// *************************************************************************
// parse a char seperated string list into TStrings
// *************************************************************************
procedure ParseSepStrToTStrings(S: string; sep : char; t : TStrings);
var
    i       : integer;
    tempstr : string;
    x       : string;
begin
    tempstr := sep;
    t.Clear;
    S := S + sep;

    while S > '' do
    begin
        i := pos(tempstr,S);
        if (i <> 0) then
        begin
            x := copy(S,1,i-1);
            t.Add(x);
        end;
        S := copy(S,i+1,length(s)-i);
    end;
end;

// *************************************************************************
// Remove a sring from TStrings
// *************************************************************************
function RemoveStringFromList(x : string; y : TStrings) : boolean;
var
    i : integer;
    p : integer;
begin
    Result := false;
    i := 0;
    while i < y.Count do
    begin
        p := y.IndexOf(x);
        if (p >= 0) and (p < y.Count) then
        begin
            y.Delete(p);
            Result := true;
        end;
        inc(i);
    end;
end;

// *************************************************************************
// Take a RSDP menu item and convert it for windows (with underlined shortcut)
// *************************************************************************
function CreateRSDPMenuTextWithShortcut(x : string) : string;
var
    c : char;
    t : string;
begin
    c := FindFirstNumericorCapitalChar(x);
    t := x;

    if c <> #0 then
    begin
        if pos('&'+c,x) = 0 then
        begin
            t := StringReplace(x,c,'&'+c,[]);
        end;
    end;
    Result := t;
end;

// *************************************************************************
// Return the text after a marker string
// *************************************************************************
function Getproperty(x : string; marker : string) : string;
var
    t : string;
begin
    t := x;
    t := StringReplace(t,marker,'',[]);
    t := trim(t);
    Result := t;
end;

end.



