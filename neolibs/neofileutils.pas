unit neofileutils;

{$mode delphi}

interface

uses
   forms, sysutils, Graphics, INIFiles, classes, neotextutils, windows,
   Dialogs, controls, comctrls, neotypedef, registry;

   // lazarus mod: filectrl removed from uses list

const
   bfReplaceExt = $01;
   bfAddExt     = $02;
   bmMoveFile   = $01;
   bmCopyFile   = $02;

   fmBold       = $0001;
   fmItalic     = $0002;
   fmUnderline  = $0004;
   fmStrikeout  = $0008;

   VSS_FILENAME = 'VSSVER.SCC';

type
   TFileCountOptions = set of (coIncludeSubDirs);
   TSearchCallBack   = procedure(x : string);

    TExtFileStream = class(TFileStream)
    public
        function Readln : string;
    end;

function  GetApplicationPath(x : TApplication) : string; overload;
function  GetApplicationPath : string;  overload;
function  GetApplicationExeName(x : TApplication) : string;
function  FilePathRelToApp(x : string; y : TApplication) : string; overload;
function  FilePathRelToApp(x : string) : string; overload;

procedure LoadFontFromINI(FN : string; x : TFont; Key : string);
procedure SaveFontToINI(FN : string; x : TFont; Key : string);
function  FindFile(Dirs : string; FN : string) : string;
procedure DirStringToTstrings(Dirs : string; DirList : TStrings);
function  CreateBackup(Filename, Extension:string;
                       Flags : cardinal; BackupMode : Cardinal) : Boolean;
function  QueryDirectoryExists(Fn : string) : Boolean;
procedure LoadTreeFromFile(FN : string; Node : TTreeNode; Tree : TTreeView; x : integer);
procedure CreateEmptyFile(FN : string; CreateDirs : Boolean = false; Prompt : Boolean = false);
procedure CheckOldStyle(FN : string);
function  LoadProgrammingFile(x : PLargeByteArray; Len : integer; FN : string; FType : integer) : integer;
function  Hex2Bin(Inf,Outf : String; FillValue : byte = $00) : Boolean;
function  Bin2Hex(Inf,Outf : String; ExclSt1, ExclEnd1, ExclSt2, ExclEnd2 : integer) : Boolean;
function  GetFileType(FN : string) : integer;
function  ChangeFileExt(FN : string; Extension : string) : string;
function  StripFileExt(FN : string) : string;
function  ExtractFileNameOnly(FN : string) : string;
function  FindPatternInFile(FN : string; RefData : PByteArray; Len : integer) : integer;
function  ReadDataAtFileOfs(FN : string; Offset : integer; Buf : PByteArray; Len : integer) : integer;
function  Reset2Read ( var F : file; PS : string ) : boolean;
function  FindPatternInFileFromEnd(FN : string; RefData : PByteArray; Len : integer) : integer;
function  IsVssFilename(FN : string) : Boolean;
function  GetFileCount(Path : string; Extension : string; Options : TFileCountOptions) : integer; overload;
function  GetFileCount(Paths : Tstrings; Extensions : TStrings; Options : TFileCountOptions) : integer; overload;
procedure readToEndOftextLine(var SF : TCharFile);
procedure ReadStringToEndOfLine(var SF : TCharFile; var x : string);
function  INIFilename : string;
procedure LoadListFromINISection(Filename : string; Section : String; ListView : TListView);
procedure SaveListToINISection(Filename : string; Section : String; ListView : TListView;
                               EraseSection : Boolean = True);
procedure SearchAllFiles(StartDir : string; Ext : string; Proc : TSearchCallBack); overload;
procedure SearchAllFiles(StartDir : string; Ext : string; Attr : integer; Proc : TSearchCallBack); overload;
procedure CheckDir(Dir : string);
procedure WriteTStringsToINI(FN : string; x : TStrings; Section : string;
                     EraseSection : Boolean = true; IgnoreBlanks : Boolean = true);
procedure ReadTStringsFromINI(FN : string; x : TStrings; Section : string);
procedure LoadContentsFromFile(FN : string; Node : TTreeNode; Tree : TTreeView; x : integer);
function  CheckSubStringInHTML(InFile : string; Text: string) : Boolean;
procedure SearchAndHighLiteHTML(InFile : string; OutFile : string; Text : string);
procedure FindAndReplaceInFile(InFn,outFN : string; ReplaceList:TReplaceList);
function  FindParam ( PStr : string; CaseMatch, Partial : boolean ) : byte;
function  FindParamStr ( PStr : string; CaseMatch : boolean ) : string; overload;
function  FindParamStr(PStr : string; CaseMatch : Boolean; var x : string) : Boolean; overload;
function  FindParamInt(PStr : string; CaseMatch : Boolean; var x : integer) : Boolean;
function  FindParamHexInt(PStr : string; CaseMatch : Boolean; var x : integer) : Boolean;
function  GetWindowsDir : string;
procedure ChangeDirToApp;
procedure AddTotextFile(Fn : string; Txt : string; NewLine : Boolean = true);
function  ChkHexRecordChecksum(x : string) : Boolean;
function  GetHexRecordType(x : string) : integer;
function  GetHexRecordOffset(x : string) : integer;
function  GetHexExtLinAddr(x : string) : cardinal;
function  AddCommonPathToEnv : Boolean;
function  FilePathUpDir(fp : string) : string;
function  GetProgramFilesDir : string;
function  FilePathAndName(FN : string) : string; // strips away the drive letter part
function  GetDriveLetter(FN : string) : string; // returns the drive letter
function  WriteByteValToFile(var FH: file; Num : integer; val : byte) : Boolean;
function  GetIniFilename : string;
function  CreateDirectories(path : string) : boolean;

implementation

// ***********************************************************
// Read a text line from a file
// ***********************************************************
function TExtFileStream.Readln : string;
var
    s : string;
    t : byte;
begin
    s := '';

    t := 0;
    // advance to next text char
    while (not IsPrintable(t)) and (Position < (Size-1) ) do t := ReadByte;

    s := s + chr(t);
    // advance to end of text
    while IsPrintable(t) and (Position < (Size-1) ) do
    begin
        t := ReadByte;
        if (IsPrintable(t)) then s := s + chr(t);
    end;

    Result := s;
end;

// ***********************************************************
// Get the path of the application
// ***********************************************************
function GetApplicationPath(x : TApplication) : string;
begin
   GetApplicationPath := ExtractFilePath(x.ExeName);
end;

// ***********************************************************
// Get the path of this application
// ***********************************************************
function  GetApplicationPath : string;
begin
   GetApplicationPath := ExtractFilePath(Application.ExeName);
end;

// ***********************************************************
// Get the executable name of the application
// ***********************************************************
function GetApplicationExeName(x : TApplication) : string;
begin
   GetApplicationExeName := ExtractFileName(x.ExeName);
end;

// ***********************************************************
// Append the filename of x to the path of the application
// ***********************************************************
function FilePathRelToApp(x : string; y : TApplication) : string;
begin
   FilePathRelToApp := GetApplicationPath(y) + x;
end;

// ***********************************************************
// Append the filename of x to the path of this application
// ***********************************************************
function FilePathRelToApp(x : string) : string;
begin
   FilePathRelToApp := GetApplicationPath(Application) + x;
end;

// ***********************************************************
// Load a font from an INI file
// ***********************************************************
procedure LoadFontFromINI(FN : string; x : TFont; Key : string);
var
  INIFile : TINIFile;
  temp    : integer;
begin
  INIFile := TINIFile.Create(FN);

  with INIFile do
  begin
    x.CharSet := ReadInteger(Key,'CharSet',0);
    x.Color   := ReadInteger(Key,'Color',clBlack);
    x.Height  := ReadInteger(Key,'Height',10);
    x.Name    := ReadString(Key,'Name','Arial');
    x.Pitch   := TFontPitch(ReadInteger(Key,'Pitch',integer(fpDefault)));
    x.Size    := ReadInteger(Key,'Size',10);

    temp      := ReadInteger(Key,'Style',0);

    x.Style := [];
    if (temp and fmBold) <> 0      then x.Style := x.Style + [fsBold];
    if (temp and fmItalic) <> 0    then x.Style := x.Style + [fsItalic];
    if (temp and fmUnderline) <> 0 then x.Style := x.Style + [fsUnderline];
    if (temp and fmStrikeout) <> 0 then x.Style := x.Style + [fsStrikeout];
  end;

  INIFile.Free;
end;

// ***********************************************************
// Load a font from an INI file
// ***********************************************************
procedure SaveFontToINI(FN : string; x : TFont; Key : string);
var
  INIFile : TINIFile;
  Temp    : integer;
begin
  INIFile := TINIFile.Create(FN);

  with INIFile do
  begin
    WriteInteger(Key,'CharSet',x.CharSet);
    WriteInteger(Key,'Color',x.Color);
    WriteInteger(Key,'Height',x.Height);
    WriteString(Key,'Name',x.Name);
    WriteInteger(Key,'Pitch',integer(x.Pitch));
    WriteInteger(Key,'Size',x.Size);

    Temp := 0;
    if fsBold      in x.Style then Temp := Temp + fmBold;
    if fsUnderline in x.Style then Temp := Temp + fmUnderline;
    if fsStrikeout in x.Style then Temp := Temp + fmStrikeout;
    if fsItalic    in x.Style then Temp := Temp + fmItalic;

    WriteInteger(Key,'Style',temp);
  end;

  INIFile.Free;
end;

// ***********************************************************
// find a file in directories
// ***********************************************************
procedure FileSearchDir(FN: string; dir : string; var Res : string);
var
   FileFound : TSearchRec;
   temp      : integer;
begin
   temp := FindFirst(dir+'*.*',faAnyFile,FileFound);

   while temp = 0 do
   begin
      if (FileFound.name <> '.') and (FileFound.name <> '..') then
      begin
         if (FileFound.Attr and faDirectory) <> 0 then
            FileSearchDir(FN,dir+FileFound.name+'\',Res)
         else if FileFound.name = FN then
            res := Res + dir + FN + ';';
      end;
      temp := FindNext(FileFound);
   end;

end;

// ***********************************************************
// Strings list of directories
// ***********************************************************
procedure DirStringToTstrings(Dirs : string; DirList : TStrings);
var
   temp    : integer;
   tempstr : string;
   i       : integer;
begin
  DirList.Clear;

  temp := pos(';',Dirs);
  while( temp <> 0) do
  begin
     tempstr := Copy(Dirs,0,temp-1);
     DirList.Add(tempstr);

     if  GetNumOccurencesOfChar(Dirs,';') = 1 then
        Dirs := Copy(Dirs,temp+1,Length(dirs)-1-length(tempstr))
     else
        Dirs := Copy(Dirs,temp+1,Length(dirs)-2-length(tempstr));

     temp := pos(';',Dirs);
  end;
  DirList.Add(Dirs);

  i := 0;
  while i < DirList.Count -1 do
  begin
     tempstr := ExtractFileDir(DirList.Strings[i]);
     if not DirectoryExists(TempStr) then
        DirList.Delete(i);
     inc(i);
  end;
end;

// ***********************************************************
// Find a file from a list of directories
// ***********************************************************
function FindFile(Dirs : string; FN : string) : string;
var
   i       : integer;
   res     : string;
   dirlist : TStringList;
begin
   DirList := TStringList.Create;
   DirStringToTStrings(Dirs,DirList);
   if DirList.Count = 0 then
   begin
      FindFile := '';
      exit;
   end;
   res := '';

   for i:=0 to DirList.Count-1 do
   begin
      FileSearchDir(ExtractFilename(FN),DirList.Strings[i],res);
   end;

   FindFile := Res;
end;

// ***********************************************************
// Create a backup
// ***********************************************************
function  CreateBackup(Filename, Extension:string;
                       Flags : cardinal; BackupMode : Cardinal) : Boolean;
var
   BackupFN : string;
begin
   BackupFN := Filename;
   if (Flags and bfReplaceExt) <> 0 then
   begin
      BackupFN := StringReplace(BackupFN,ExtractFileExt(FileName),Extension,[rfIgnorecase])
   end;

   if (Flags and bfAddExt) <> 0 then
   begin
      BackupFN := BackupFN + Extension
   end;

   if FileExists(BackupFN) then DeleteFile(pAnsiChar(BackupFN));
   if (BackupMode and bmMoveFile) <> 0 then
      CreateBackup := moveFile(pAnsiChar(FileName),pAnsiChar(backupFN))
   else if (BackupMode and bmCopyFile) <> 0 then
      CreateBackup := CopyFile(pAnsiChar(FileName),pAnsiChar(backupFN),false)
   else
   begin
      CreateBackup := False;
      MessageDlg('Warning: CreateBackup called without a valid BackupMode parameter!',
            mtWarning,[mbOK],0);
   end;
end;

// ***********************************************************
// Check if the directory exists and query the user to create
// or not
// ***********************************************************
function  QueryDirectoryExists(Fn : string) : Boolean;
var
   tempDir  : string;
begin
   QueryDirectoryExists := False;
   tempDir := ExtractFilePath(FN);
   if tempDir = '' then Exit;

   QueryDirectoryExists := True;
   if DirectoryExists(tempdir) then Exit;

   if MessageDlg('The directory of the path/name "'+ Fn +'" does not exist! Do you wish to create?',
            mtConfirmation,[mbYes,mbNo],0) = mrYes then
     QueryDirectoryExists := CreateDir(tempdir)
   else
     QueryDirectoryExists := False;
end;

// ***********************************************************
// Load a subndes froma file
// ***********************************************************
procedure LoadTreeFromFile(FN : string; Node : TTreeNode; Tree : TTreeView; x : integer);
const
   MAX_LEVELS = 100;
var
   FH   : TextFile;
   Temp : string;

   State   : Integer;
   indent  : integer;
   CurLev  : integer;
   Nodes   : Array[0..MAX_LEVELS] of TTreeNode;

   LastChildAdded : TTreeNode;
begin
   try
      AssignFile(FH,FN);
      Reset(FH);
      State    := 0;
      Nodes[0] := Node;

      LastChildAdded := Nodes[0];

      CurLev   := 0;

      while not eof(FH) do
      begin
         readln(FH,temp);

         if State = 0 then
         begin
            if pos('$TableTreedefs:',temp)<>0 then
               State := 1
         end
         else if State = 1 then
         begin
            if pos('$',temp) <> 0 then
                State := 2
            else
            begin
               indent := CharCount(temp,CHR_TAB,1);
               temp   := TrimCharLeft(temp,CHR_TAB);
               if indent > CurLev then
               begin
                  if indent > MAX_LEVELS then
                  begin
                     MessageDlg('The "LoadTreeFromFile" procedure is compiled to handle a '+
                          'tree with a depth of no more than '+SysUtils.IntToStr(MAX_LEVELS)+'. The current '+
                          'level loaded from the file is '+SysUtils.IntToStr(indent)+'. The procedure will default to '+
                          SysUtils.IntToStr(MAX_LEVELS)+'.',mtWarning,[mbOK],0);
                     indent := MAX_LEVELS;
                  end;
                  CurLev := indent;
                  Nodes[CurLev] := LastChildAdded;
               end
               else if (indent < Curlev) then
               begin
                  if indent < 0 then indent := 0;

                  CurLev := indent;
               end;

               LastChildAdded := Tree.Items.AddChild(Nodes[CurLev],temp);
               LastChildAdded.ImageIndex := x;
               LastChildAdded.SelectedIndex := x;

            end;
         end;
      end;

   finally
      CloseFile(FH);
   end;
end;

// ***********************************************************
// Create an empty file on disk
// ***********************************************************
procedure CreateEmptyFile(FN : string; CreateDirs : Boolean = false; Prompt : Boolean = false);
var
   FH : TextFile;
   Temp : string;
begin
   if CreateDirs then
   begin
      if not DirectoryExists(FN) then
      begin
         if Prompt then
         begin
            if MessageDlg('Create directory for '+FN+' ?',mtConfirmation,[mbYes, mbNo],0) = mrYes then
            begin
               temp := ExtractFilePath(FN);
               CreateDir(temp);
            end;
         end
         else
         begin
            temp := ExtractFilePath(FN);
            CreateDir(temp);
         end;
      end;
   end;

   AssignFile(FH,FN);
   {$I-}
    reWrite(FH);
   {$I+}
   if IOResult <> 0 then
   begin
      if Prompt then
      begin
         MessageDlg('Could not create file '+FN+' !',mtWarning,[mbOK],0);
      end;
   end;

   CloseFile(FH);
end;

// ***********************************************************
// Prepare the table for editing
// ***********************************************************
procedure CheckOldStyle(FN : string);
Var
   FH  : textFile;
   OFH : TextFile;
   tempStr : string;
   i       : integer;
   MustMove : Boolean;
begin
   try
      AssignFile(FH,FN);
      Reset(FH); // was Reset(FH,FN); -> changed 2009-10-02 for lazarus

      MustMove := False;
      readln(FH,tempStr);
      if pos('[',tempStr) <> 0 then
      begin
         MustMove := True;
         AssignFile(OFH,FN+'.new');
         Rewrite(OFH);
         while not eof(FH) do
         begin
            readln(FH,tempstr);
            if pos('[',tempstr) = 0 then
            begin
               if pos('En_',tempstr) <>0 then
               begin
                  tempstr := stringreplace(tempstr,'0','false',[]);
                  tempstr := stringreplace(tempstr,'1','true',[]);
               end;
               i := pos('=',tempstr);
               TempStr := Copy(tempstr,i+1,length(tempStr)-i);
               writeln(OFH,tempstr);
            end;
         end;
         CloseFile(OFH);
      end;
   finally
      CloseFile(FH);
   end;
   if MustMove then
   begin
      DeleteFile(pAnsiChar(FN));
      MoveFile(pAnsiChar(FN+'.new'),pAnsiChar(FN));
   end;
end;

// ***********************************************************
// Prepare the table for editing
// ***********************************************************
function LoadProgrammingFile(x : PLargeByteArray; Len : integer; FN : string; FType : integer) : integer;
var
   LoadFN    : string;
   FH        : File of Byte;
   NumLoaded : integer;
   FileLen   : integer;
begin
   LoadProgrammingFile := -1;
   LoadFN := FN;
   if FType = ftHEXFile then
   begin
       if not Hex2Bin(FN,FilePathRelToApp('Temp.$$$',Application)) then Exit;
       LoadFN := FilePathRelToApp('Temp.$$$',Application);
   end;

   try
      AssignFile(FH,LoadFN);
      Reset(FH);

      FileLen := FileSize(FH);
      if FileLen <= Len then
      begin
         Numloaded := 0;
         BlockRead(FH,x^,FileLen,Numloaded);
         LoadProgrammingFile := NumLoaded;
      end;
   finally
      CloseFile(FH);
   end;
end;

// ***********************************************************
// get the intel Hex record length from a string
// ***********************************************************
function  GetHexRecordLen(x : string) : integer;
begin
   GetHexrecordLen := HexToInt(copy(x,2,2));
end;

// ***********************************************************
// get the intel Hex record type from a string
// ***********************************************************
function  GetHexRecordType(x : string) : integer;
begin
   GetHexrecordType := HexToInt(copy(x,8,2));
end;

// ***********************************************************
// get the intel Hex load offset from a string
// ***********************************************************
function  GetHexRecordOffset(x : string) : integer;
begin
   GetHexrecordOffset := HexToInt(copy(x,4,4));
end;

// ***********************************************************
// Add intel HEX checksum to string
// ***********************************************************
function AddHexChecksum(x : string) : string;
var
   Chk     : integer;
   i       : integer;
begin
   Chk := 0;
   for i := 1 to ((length(x)) div 2) do
      Chk := Chk + HexToInt(Copy(x,i*2,2));
   Chk := Chk and $FF;
   Chk := Chk xor $FF;
   Chk := Chk + 1;
   Chk := Chk and $FF;
   AddHexChecksum := x + IntToHex(Chk,2);
end;

// ***********************************************************
// get the intel Hex checksum from a string
// ***********************************************************
function  ChkHexRecordChecksum(x : string) : Boolean;
var
   Chk     : integer;
   TempChk : integer;
   i       : integer;
begin
   ChkHexRecordChecksum := False;

   TempChk := HexToInt(copy(x,length(x)-1,2));

   Chk := 0;
   for i := 1 to ((length(x) - 2) div 2) do
      Chk := Chk + HexToInt(Copy(x,i*2,2));
   Chk := Chk and $FF;
   Chk := Chk xor $FF;
   Chk := Chk + 1;
   Chk := Chk and $FF;
   if Chk = TempChk then ChkHexRecordChecksum := True;
end;

// ***********************************************************
// get the intel Hex checksum from a string
// ***********************************************************
function GetHexExtLinAddr(x : string) : cardinal;
begin
   GetHexExtLinAddr := HexToInt(copy(x,10,4));
end;

// ***********************************************************
// Hex to bin conversion
// ***********************************************************
function  Hex2Bin(Inf,Outf : String; FillValue : byte = $00) : Boolean;
type
   THexBufPtr = T4MBBufPtr;
   THexBuf    = T4MBBuf;
var
   TempStr   : string;
   IFH       : TextFile;
   OFH       : File of Byte;
   CurBase   : Cardinal;
   // reclen  : Integer;
   recType   : integer;
   recOffs   : integer;
   Buf       : THexBufPtr;
   i         : integer;
   AbsAddr   : cardinal;
   EndOfFile : Boolean;

   HighestAddr : Cardinal;
begin
   Hex2Bin     := False;
   CurBase     := 0;
   EndofFile   := False;
   HighestAddr := 0;

   if not FileExists(Inf) then Exit;

   new(Buf);
   for i := 0 to high(THexBuf)-1 do Buf[i] := FillValue;
   try
      AssignFile(IFH,Inf);
      Reset(IFH);

      AssignFile(OFH,OutF);
      Rewrite(OFH);

      while (not eof(IFH)) and (EndOfFile = False) do
      begin
         ReadLn(IFH,TempStr);
         if (TempStr[1] = ':') and (ChkHexRecordChecksum(tempStr)) then
         begin
            // RecLen  := GetHexRecordLen(TempStr);
            RecType := GetHexrecordType(TempStr);
            recOffs := GetHexRecordOffset(TempStr);

            case RecType of
              $00 : // Data record
                    begin
                       i := 10;
                       while(i<(length(TempStr)-2)) do
                       begin
                          AbsAddr := Curbase + cardinal(recOffs) + cardinal((i - 10) div 2);
                          if Absaddr > HighestAddr then
                              HighestAddr := AbsAddr;
                          if AbsAddr < sizeof(THexBuf) then
                              Buf^[AbsAddr] := HexToInt(Copy(Tempstr,i,2));
                          i := i + 2;
                       end;
                    end;
              $01 : // end of file record
                    EndOfFile := True;
              $02 : CurBase := GetHexExtLinAddr(TempStr) shl 4;
              $03 : CurBase := (GetHexExtLinAddr(TempStr) shl 4) +
                               Cardinal(HexToInt(copy(TempStr,14,4)));
              $04 : CurBase := GetHexExtLinAddr(TempStr) shl 16;
              $05 : CurBase := HexToInt(copy(tempStr,10,8));
            end;
         end
         else
            MessageDlg('Fail',mtWarning,[mbok],0);
      end;
      BlockWrite(OFH,Buf^,HighestAddr+1);
   finally
      CloseFile(IFH);
      CloseFile(OFH);
   end;
   dispose(Buf);
   Hex2Bin := True;
end;

// ***********************************************************
// Hex to bin conversion
// ***********************************************************
function  Bin2Hex(Inf,Outf : String; ExclSt1, ExclEnd1, ExclSt2, ExclEnd2 : integer) : Boolean;
type
   THexBufPtr = T1MBBufPtr;
   THexBuf    = T1MBBuf;
var
   OFH       : TextFile;
   IFH       : File of Byte;
   TempBuf   : THexBufPtr;
   NumRead   : integer;
   i,j,k     : integer;
   temp      : integer;
   TempLine  : string;
   TempAddr  : integer;
begin
   new(TempBuf);

   AssignFile(IFH,Inf);
   {$I-}
   Reset(IFH);
   {$I+}
   if IOResult = 0 then
   begin
      NumRead := 0;
      BlockRead(IFH,TempBuf^,sizeof(THexBuf),NumRead);
   end;
   CloseFile(IFH);

   AssignFile(OFH,OutF);
   {$I-}
   Rewrite(OFH);
   {$I+}
   if IOResult = 0 then
   begin
      for i := 0 to (NumRead div $10000) do
      begin
         temp := (i*$10000) + $FFFF;
         if temp > (NumRead-1) then temp := NumRead-1;
         Templine := ':02000004'+IntToHex((i*$10000) shr 16,4);
         TempLine := AddHexChecksum(TempLine);
         WriteLn(OFH,TempLine);
         for j := ((i*$10000) div 16) to (temp div 16) do
         begin
            TempAddr := j * 16;
            if not (   ((TempAddr >= ExclSt1) and (TempAddr <= ExclEnd1)
                    or  (TempAddr >= ExclSt2) and (TempAddr <= ExclEnd2)) ) then
            begin
               templine := ':10' + IntToHex((j*16) and $FFFF,4) + '00';
               for k := 0 to 15 do
               begin
                  TempLine := TempLine + IntToHex(TempBuf[(j*16) + k],2);
               end;
               TempLine := AddHexChecksum(TempLine);
               WriteLn(OFH,TempLine);
            end;
         end;
      end;
   end;
   writeln(OFH,':00000001FF');
   CloseFile(OFH);
   Bin2Hex := True;
end;

// ***********************************************************
// Return the type of the file
// ***********************************************************
function  GetFileType(FN : string) : integer;
var
   tempstr : string;
begin
   tempstr := Uppercase(ExtractFileExt(FN));

        if tempstr = '.HEX' then GetFileType := ftHEXFile
   else if tempstr = '.H86' then GetFileType := ftHEXFile
   else if tempstr = '.BIN' then GetFileType := ftBINFile
   else GetFileType := ftBINFile;
end;

// ***********************************************************
// Change a file extension
// ***********************************************************
function  ChangeFileExt(FN : string; Extension : string) : string;
var
   temp    : integer;
   tempstr : string;
begin
   temp    := pos('.',FN);
   tempstr := copy(FN,1,temp);
   tempstr := tempstr + Extension;
   ChangeFileExt := tempstr;
end;

// ***********************************************************
// Strip the file extension from the filename
// ***********************************************************
function  StripFileExt(FN : string) : string;
var
   Temp : integer;
begin
   Temp := FindLast('.',FN);
   StripFileExt := Copy(FN,1,Temp-1);
end;

// ***********************************************************
// Extract the name only (no path and no extension)
// ***********************************************************
function  ExtractFileNameOnly(FN : string) : string;
var
   TempStr : string;
   i       : integer;
begin
   TempStr  := ExtractFileName(FN);
   i        := pos('.',TempStr);
   if i = 0 then i := length(TempStr);
   ExtractFileNameOnly := copy(TempStr,1,i-1);
end;

// ***********************************************************
// Find the offset of a specific pattern in a file
// ***********************************************************
function FindPatternInFile(FN : string; RefData : PByteArray; Len : integer) : integer;
var
   FH    : File;
   Buf   : PByteArray;
   Ofs   : integer;
   Amt   : integer;
   found : Boolean;
begin
   Result := -1;
   {$HINTS OFF}
   if not Reset2Read(FH,FN) then
      Exit;
   {$HINTS ON}

   Buf    := Allocmem(Len+1);
   Ofs    := 0;
   Found  := False;

   while (not eof(FH)) and (not Found) do
   begin
      seek(FH,Ofs);
      Amt := 0;  // Lazarus mod
      BlockRead(FH,Buf^,Len,Amt);
      if Amt >= Len then
      begin
         if CompareMem(Buf,RefData,Len) then
         begin
            Result := Ofs;
            Found  := True;
         end;
      Ofs := Ofs + 1;
      end;
   end;
   CloseFile(FH);
   Dispose(Buf);
end;

// ***********************************************************
// Find the offset of a specific pattern in a file
// ***********************************************************
function FindPatternInFileFromEnd(FN : string; RefData : PByteArray; Len : integer) : integer;
var
   FH    : File;
   Buf   : PByteArray;
   Ofs   : integer;
   Amt   : integer;
   found : Boolean;
begin
   Result := -1;
   {$HINTS OFF}
   if not Reset2Read(FH,FN) then
      Exit;
   {$HINTS ON}

   Buf    := Allocmem(Len+1);
   Ofs    := FileSize(FH);
   found  := False;

   while (Ofs >= 0) and (not found) do
   begin
      seek(FH,Ofs);
      Amt := 0;  // Lazarus mod
      BlockRead(FH,Buf^,Len,Amt);
      if Amt >= Len then
      begin
         if CompareMem(Buf,RefData,Len) then
         begin
            Result := Ofs;
            Found  := True;
         end;
      end;
      Ofs := Ofs - 1;
   end;
   CloseFile(FH);
   Dispose(Buf);
end;

// ***********************************************************
// Read data into a buffer at a specific offset in a file
// ***********************************************************
function ReadDataAtFileOfs(FN : string; Offset : integer; Buf : PByteArray; Len : integer) : integer;
var
   FH  : File;
   Amt : integer;
begin
   Result := -1;
   {$HINTS OFF}
   if not Reset2Read(FH,FN) then
      Exit;
   {$HINTS ON}

   seek(FH,Offset);
   Amt := 0;  // Lazarus mod
   BlockRead(FH,Buf^,Len,Amt);
   Result := Amt;

   CloseFile(FH);
end;

// ***********************************************************
// Reset to read-only mode
// ***********************************************************
function Reset2Read ( var F : file; PS : string ) : boolean;
var
   SaveFM : byte;
begin
   SaveFM       := FileMode;       {Save the current FileMode status}
   FileMode     := 0;              {The default is 2}

   assign (F, PS);
   {$I-}
   reset (F,1);
   {$I+}
   Reset2Read := (IoResult = 0);

   FileMode     := SaveFM;         {Restore the original FileMode}
end;

// ***********************************************************
// Test if the filename is a VSS source safe filename
// ***********************************************************
function IsVssFilename(FN : string) : Boolean;
begin
   if UpperCase(ExtractFileName(FN)) = VSS_FILENAME then
      Result := True
   else
      Result := False;
end;

// ***********************************************************
// Count the number of files in a path
// ***********************************************************
function  GetFileCount(Path : string; Extension : string; Options : TFileCountOptions) : integer;
var
   Res     : TSearchRec;
   i       : integer;
   tempExt : string;
begin
   Result := 0;
   i := FindFirst(Path+'\*.*',faAnyFile,Res);
   while i=0 do
   begin
      if (Res.Name <> '.') and (Res.Name <> '..') then
      begin
         if (Res.Attr and faDirectory) <> 0 then
         begin
            if coIncludeSubDirs in Options then
               result := Result + getFileCount(Path+'\'+Res.name, Extension,Options);
         end
         else
         begin
            tempExt := '*'+ExtractFileExt(Res.name);
            if (Uppercase(tempExt) = UpperCase(Extension)) then
               Result := Result + 1;
         end;
      end;
      i := FindNext(Res);
   end;
end;

// ***********************************************************
// Count the number of files in a path
// ***********************************************************
function  GetFileCount(Paths : Tstrings; Extensions : TStrings; Options : TFileCountOptions) : integer;
var
   i,j : integer;
begin
   result := 0;
   for i := 0 to Paths.Count-1 do
      for j := 0 to Extensions.Count-1 do
         Result := Result + GetFileCount(Paths.Strings[i],Extensions.Strings[i],Options);
end;

// ***********************************************************
// Count the number of files in a path
// ***********************************************************
procedure readToEndOftextLine(var SF : TCharFile);
var
   TempChr : Char;
begin
   while not eof(SF) do
   begin
      read(SF,TempChr);
      if (TempChr = CHR_LF) or (TempChr = CHR_CR) then Exit;
   end;
end;

// ***********************************************************
// Read a string to the end of line
// ***********************************************************
procedure ReadStringToEndOfLine(var SF : TCharFile; var x : string);
var
   TempChr : Char;
begin
   if not eof(SF) then x := '';

   while not eof(SF) do
   begin
      read(SF,TempChr);
      if (TempChr = CHR_LF) or (TempChr = CHR_CR) then
         Exit
      else
         x := x + TempChr;
   end;
end;

// ***********************************************************
// Return an INI filename with the same name as the application
// but a different extension
// ***********************************************************
function  INIFilename : string;
begin
   Result := Application.ExeName;
   Result := StringReplace(Result,'.EXE','.INI',[rfIgnorecase]);
end;

// ***********************************************************
// Load the entries of a section into a listview
// ***********************************************************
procedure LoadListFromINISection(Filename : string; Section : String; ListView : TListView);
var
   INIFile      : TINIFile;
   i            : integer;
   NewListItem  : TListItem;
   StringList   : TStringList;
   TempStr      : string;
   TempStr1     : string;
   TempStr2     : string;
begin
   INIFile      := TINIFile.Create(FileName);
   StringList   := TStringList.Create;

   ListView.Items.Clear;
   INIFile.ReadSectionValues(Section,StringList);

   NewlistItem := nil;

   if StringList.Count <> 0 then
      for i := 0 to StringList.Count-1 do
      begin
         TempStr             := StringList.Strings[i];
         TempStr1 := ''; // Lazarus mod
         TempStr2 := ''; // Lazarus mod
         SplitString('=',TempStr,TempStr1,TempStr2);
         if pos('Key',TempStr1) <> 0 then
         begin
            NewListItem         := ListView.Items.Add;
            NewListItem.caption := TempStr2;
         end
         else if pos('SubItem',TempStr1) <> 0 then
         begin
            if NewListItem <> nil then
               NewListItem.SubItems.Add(TempStr2);
         end
      end;

   INIFile.Free;
   StringList.Free;
end;

// ***********************************************************
// Save listview to INIfile section
// ***********************************************************
procedure SaveListToINISection(Filename : string; Section : String; ListView : TListView;
                               EraseSection : Boolean = True);
var
   INIFile      : TINIFile;
   i,j          : integer;
   Key          : string;
   Value        : string;
   TempListItem : TListItem;
begin
   INIFile      := TINIFile.Create(FileName);

   if EraseSection then
      INIFile.EraseSection(Section);

   if ListView.Items.count <> 0 then
   begin
      for i := 0 to ListView.Items.count-1 do
      begin
         TempListItem := ListView.Items[i];

         Key   := 'Key'+SysUtils.IntToStr(i);
         Value := TempListItem.Caption;
         INIFile.WriteString(Section,key,Value);

         Key := 'SubCount'+SysUtils.IntToStr(i);
         INIFile.WriteInteger(Section,Key,TempListItem.SubItems.Count);

         if (TempListItem.SubItems.Count <> 0) then
         begin
            for j:=0 to TempListItem.SubItems.Count-1 do
            begin
               Key := 'SubItem'+SysUtils.IntToStr(i)+'_'+SysUtils.IntToStr(j);
               INIFile.WriteString(Section,Key,TempListItem.SubItems.Strings[j]);
            end;
         end;
      end;
   end;

   INIFile.Free;
end;

// ***********************************************************
// Search a directory and its subdirectories for a file type.
// If found call a specific routine
// ***********************************************************
procedure SearchAllFiles(StartDir : string; Ext : string; Proc : TSearchCallBack);
begin
   SearchAllFiles(StartDir,Ext,faAnyFile,Proc);
end;

// ***********************************************************
// Search a directory and its subdirectories for a file type.
// If found call a specific routine
// ***********************************************************
procedure SearchAllFiles(StartDir : string; Ext : string; Attr : integer; Proc : TSearchCallBack);
var
   SearchRec : TSearchRec;
   temp      : integer;
begin
   temp := FindFirst(StartDir + '\*.*',Attr,SearchRec);
   while (temp = 0) do
   begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
         if ((SearchRec.Attr and faDirectory) <> 0) then
         begin
            SearchAllFiles(StartDir + '\' + SearchRec.name,Ext,Proc);
         end
         else if (UpperCase(ExtractFileExt(SearchRec.name)) = UpperCase(ExtractFileExt(Ext))) then
         begin
            if Assigned(Proc) then
               Proc(StartDir + '\' + SearchRec.Name);
         end;
      end;
      temp := FindNext(SearchRec);
   end;
   SysUtils.FindClose(SearchRec);
end;

// ***********************************************************
// Search a directory and its subdirectories for a file type.
// ***********************************************************
procedure CheckDir(Dir : string);
begin
  if not DirectoryExists(Dir) then
    if not CreateDir(Dir) then
       raise Exception.Create('Cannot create '+dir);
end;

// ***********************************************************
// Write a TStrings component to an INI File
// ***********************************************************
procedure WriteTStringsToINI(FN : string; x : TStrings; Section : string;
                     EraseSection : Boolean = true; IgnoreBlanks : Boolean = true);
var
   INIFile : TINIfile;
   i       : integer;
   temp    : TStringList;
   ofs     : integer;
begin
   INIFile := TINIFile.Create(FN);

   if EraseSection then
   begin
      Ofs := 0;
      INIFile.EraseSection(Section);
   end
   else
   begin
      temp := TStringList.Create;
      INIFile.ReadSection(Section,temp);
      Ofs  := temp.count;
   end;

   if x.count<>0 then
      for i := 0 to x.count-1 do
      begin
         if IgnoreBlanks then
         begin
            if x.Strings[i] <> '' then
               INIFile.WriteString(Section,'Line'+SysUtils.IntToStr(i+ofs),x.Strings[i]);
         end
         else
         begin
            INIFile.WriteString(Section,'Line'+SysUtils.IntToStr(i+ofs),x.Strings[i]);
         end;
      end;

   INIFile.Free;
end;

// ***********************************************************
// Read a TStrings component from an INI File
// ***********************************************************
procedure ReadTStringsFromINI(FN : string; x : TStrings; Section : string);
const
   EndStr = '!DOES NOT EXIST!@#$%^&*()';
var
   INIFile : TINIfile;
   i       : integer;
   temp    : string;
begin
   INIFile := TINIFile.Create(FN);

   i := 0;
   temp := INIFile.ReadString(Section,'Line'+SysUtils.IntToStr(i),EndStr);
   while temp <> EndStr do
   begin
      x.Add(Temp);
      inc(i);
      temp := INIFile.ReadString(Section,'Line'+SysUtils.IntToStr(i),EndStr);
   end;


   INIFile.Free;
end;

// ***********************************************************
// Load a subndes froma file
// ***********************************************************
procedure LoadContentsFromFile(FN : string; Node : TTreeNode; Tree : TTreeView; x : integer);
const
   LCFF_MAX_LEVELS = 100;
var
   FH               : TextFile;
   Temp,NodeCaption : string;
   Filename         : ^string;

   Buffer  : string;
   indent  : integer;
   CurLev  : integer;
   temppos : integer;

   Nodes          : array [0..LCFF_MAX_LEVELS] of TTreeNode;
   LastChildAdded : TTreeNode;
begin
   try
   begin
      AssignFile(FH,FN);
      Reset(FH);

      Nodes[0] := Node;

      LastChildAdded := Nodes[0];

      CurLev   := 0;

      while (not eof(FH)) do
      begin
         readln(FH,Buffer);
         Temp := Buffer;

         indent := CharCount(Temp,CHR_TAB,1);
         Temp   := TrimCharLeft(Temp,CHR_TAB);

         temppos     := pos('==',Temp);
         if TempPos <> 0 then
         begin
            NodeCaption := copy(Temp,1,temppos-1);
            Temp        := copy(Temp,temppos+2,length(Temp)-temppos-1);
            new(Filename);
            Filename^ := Temp;
         end
         else
         begin
            NodeCaption := Temp;
            FileName := nil;
         end;

         if (indent > CurLev) then
         begin
            if (indent > LCFF_MAX_LEVELS) then
            begin
               MessageDlg('The LoadContentsFromFile procedure is compiled to handle a )'+
                    'tree with a depth of no more than '+SysUtils.IntToStr(LCFF_MAX_LEVELS)+'. The current '+
                    'level loaded from the file is '+SysUtils.IntToStr(indent)+'. The procedure will default to '+
                    SysUtils.IntToStr(LCFF_MAX_LEVELS)+'.',mtWarning,[mbOK],0);
               indent := LCFF_MAX_LEVELS;
            end;
            CurLev := indent;
            Nodes[CurLev] := LastChildAdded;
         end
         else if (indent < CurLev) then
         begin
            if (indent < 0) then
            begin
               indent := 0;
            end;

            CurLev := indent;
         end;

         LastChildAdded               := Tree.Items.AddChild(Nodes[CurLev],NodeCaption);
         LastChildAdded.Data          := Filename;
         LastChildAdded.ImageIndex    := x;
         LastChildAdded.SelectedIndex := x;
      end;
   end;
   finally
      CloseFile(FH);
   end;
end;

// ***********************************************************
// Check if a substring exists in an HTML file
// ***********************************************************
function CheckSubStringInHTML(InFile : string; Text: string) : Boolean;
var
    InF    : TextFile;
    Buffer : string;

    TempStr : string;
begin
    Result := False;

    if not FileExists(InFile) then  Exit;

    AssignFile(InF,InFile);
    Reset(InF);

    while not eof(InF) do
    begin
       readln(InF,Buffer);
       TempStr := Buffer;

       if Pos(UpperCase(Text),UpperCase(TempStr)) <> 0 then
       begin
          Result := true;
       end;
    end;
    CloseFile(Inf);

end;

// ***********************************************************
// Search and highlight text in HTML file
// ***********************************************************
procedure SearchAndHighLiteHTML(InFile : string; OutFile : string; Text : string);
var
    Inf, OutF : TextFile;
    Buffer    : string;
    TempStr   : string;
    TempStr2  : string;
    OutStr    : string;
    temp      : integer;
begin
    CopyFile(pchar(InFile),pchar(OutFile),false);
    Exit;

    if (not FileExists(InFile)) then
    begin
       MessageDlg('Problem in SearchAndHighLiteHTML: File ' + InFile +
              ' does not exist.',mtWarning,[mbOK],0);
       Exit;
    end;

    AssignFile(InF,InFile);
    AssignFile(OutF,OutFile);

    reset(InF);
    rewrite(Outf);

    while not eof(InF) do
    begin
       readln(InF,Buffer);
       TempStr := Buffer;

       OutStr := '';
       temp := Pos(UpperCase(Text),UpperCase(TempStr));
       while (temp <> 0) do
       begin
           if ((temp <> 1) and (Pos('#'+UpperCase(Text),UpperCase(TempStr)) = (temp-1))) then
           begin
               OutStr  := OutStr + Copy(TempStr,1,temp+length(Text)-1);
               TempStr := Copy(TempStr,temp+Length(Text),Length(TempStr)-temp+1+Length(Text)+1+1);
           end
           else if Pos('NAME="'+UpperCase(Text),UpperCase(TempStr)) = (temp-6) then
           begin
               OutStr  := OutStr + Copy(TempStr,1,temp+Length(Text)-1);
               TempStr := Copy(TempStr,temp+Length(Text),Length(TempStr)-temp+1+Length(Text)+1+1);
           end
           else if Pos('HREF="'+UpperCase(Text),UpperCase(TempStr)) = (temp-6) then
           begin
               OutStr  := OutStr + Copy(TempStr,1,temp+Length(Text)-1);
               TempStr := Copy(TempStr,temp+Length(Text),length(TempStr)-temp+1+Length(Text)+1+1);
           end
           else
           begin
               TempStr2 := '<SPAN STYLE="background: #0000ff"><FONT COLOR="#ffffff">' +
                      Text + '</FONT></SPAN>';
               TempStr := StringReplace(TempStr,Text,TempStr2,[rfIgnoreCase]);
               temp    := Pos(TempStr2,TempStr);
               OutStr  := Copy(TempStr,1,temp+Length(TempStr2)-1);
               TempStr := Copy(TempStr,temp+Length(TempStr2),Length(TempStr)-temp+1+Length(Text)+1+1);
           end;
           temp := Pos(UpperCase(Text),Uppercase(TempStr));
       end;
       OutStr := OutStr + TempStr;
       writeln(OutF,OutStr);
    end;

    CloseFile(Inf);
    CloseFile(Outf);
end;

// ***********************************************************
// Find and replace text in an HTML File
// Currently only for text files
// Not finsihed yet: must also have whole words, all, and ignorecase implemented
// ***********************************************************
procedure FindAndReplaceInFile(InFn,outFN : string; ReplaceList:TReplaceList);
var
   INFH  : TextFile;
   OutFH : TextFile;
   temp : string;
   i    : integer;
begin
   if ReplaceList.count = 0 then Exit;

   AssignFile(INFH,InFn);
   AssignFile(OUTFH,OutFN);
   Reset(INFH);
   Rewrite(OUTFH);

   while not eof(INFH) do
   begin
      readln(INFH,temp);
      for i := 0 to ReplaceList.count-1 do
         temp := stringreplace(Temp,ReplaceList.GetItem(i).FindText,
                                    ReplaceList.GetItem(i).ReplaceText,
                                    [rfIgnoreCase]);
      writeln(OUTFH,temp);
   end;

   CloseFile(INFH);
   CloseFile(OUTFH);

end;

// ***********************************************************************
// find a parameter
// CaseMatch = True for case sensitive, false for not case sensitive
// partial = true to match only the PSTr,
//           or false to match the whole of the parameter string
// ***********************************************************************
function FindParam(PStr : string; CaseMatch, Partial : boolean) : byte;
var I : byte;
    S : string;
begin
   FindParam := 0;
   for I := 1 to ParamCount do
   begin
      S := ParamStr(I);
      if not(CaseMatch) then
      begin
         PStr := UpperCase(PStr);
         S    := UpperCase(S);
      end;
      if Partial then S := copy(S,1,length(PStr));
      if S = PStr then
      begin
         FindParam := I;
         Break;
      end;
   end;
end;

// ***********************************************************************
// find the string associated with a command line switch
// ***********************************************************************
function FindParamStr(PStr : string; CaseMatch : Boolean) : string;
var
   Temp    : Byte;
begin
   Result := '';

   Temp := FindParam(Pstr,CaseMatch,true);
   if Temp = 0 then Exit;

   Result  := Copy(ParamStr(Temp),length(PStr)+1,length(ParamStr(temp)) - length(PStr));

end;

// ***********************************************************************
// find the string associated with a command line switch
// ***********************************************************************
function FindParamStr(PStr : string; CaseMatch : Boolean; var x : string) : Boolean;
var
   Temp    : byte;
   TempStr : String;
begin
   Result := false;

   Temp := FindParam(Pstr,CaseMatch,true);
   if Temp = 0 then Exit;

   Result := true;

   TempStr  := Copy(ParamStr(Temp),length(PStr)+1,length(ParamStr(temp)) - length(PStr));
   x := TempStr;
end;

// ***********************************************************************
// find the integer associated with a command line switch
// ***********************************************************************
function FindParamInt(PStr : string; CaseMatch : Boolean; var x : integer) : Boolean;
var
   Temp    : byte;
   TempStr : String;
   TempInt : integer;
begin
   Result := false;

   Temp := FindParam(Pstr,CaseMatch,true);
   if Temp = 0 then Exit;

   Result := true;

   TempStr  := Copy(ParamStr(Temp),length(PStr)+1,length(ParamStr(temp)) - length(PStr));
   try
      TempInt := StrToInt(TempStr);
   except
      TempInt := x;
      Result  := false;
   end;
   x := TempInt;
end;

// ***********************************************************************
// find the HEXinteger associated with a command line switch
// ***********************************************************************
function  FindParamHexInt(PStr : string; CaseMatch : Boolean; var x : integer) : Boolean;
var
   Temp    : byte;
   TempStr : String;
   TempInt : integer;
begin
   Result := false;

   Temp := FindParam(Pstr,CaseMatch,true);
   if Temp = 0 then Exit;

   Result := true;

   TempStr  := Copy(ParamStr(Temp),length(PStr)+1,length(ParamStr(temp)) - length(PStr));
   try
      TempInt := HexToInt(TempStr);
   except
      TempInt := x;
      Result  := false;
   end;
   x := TempInt;
end;

// ***********************************************************************
// return the directory where windows was installed
// ***********************************************************************
function  GetWindowsDir : string;
const
   CHAR_BUF = 1024;
var
   TempStr : array [0..CHAR_BUF-1] of char;
begin
   GetEnvironmentvariable(PChar('windir'),PChar(@TempStr[0]),CHAR_BUF);
   Result := TempStr;
end;

// ***********************************************************************
// Change the working directory to the application
// ***********************************************************************
procedure ChangeDirToApp;
var
   TempPath : string;
begin
   TempPath := ExtractFilePath(Application.ExeName);
   SetCurrentDir(TempPath);
end;

// ***********************************************************************
// Add some text to a file
// ***********************************************************************
procedure AddTotextFile(Fn : string; Txt : string; NewLine : Boolean = true);
var
   FH : TextFile;
begin
   if not FileExists(FN) then
      CreateEmptyFile(FN);

   AssignFile(FH,FN);

   {$I-}
   Append(FH);
   {$I+}
   if IOResult = 0 then
   begin
      write(FH,Txt);
      if NewLine then writeln(FH);
   end;

   CloseFile(FH);
end;

// ***********************************************************************
// Add the file path AJApps to the <Common File> path to search
// correctly for DLLs
// Apparently this only applies to the current process; ie the PATH is
// unchanged for other processes. (a side effect that actually works nice!)
// ***********************************************************************
function AddCommonPathToEnv : Boolean;
var
   CommonFilesPath : string;
   PathEnvVar      : string;
   temp            : string;
   Reg             : TRegistry;
begin
   Reg  := TRegistry.Create;
   Reg.RootKey := HKEY_LOCAL_MACHINE;
   Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion',true);

   CommonFilesPath := Reg.ReadString('CommonFilesDir');

   Reg.Free;

   PathEnvVar := ''; // Lazarus mod
   if not GetEnvVar('Path',PathEnvVar) then
   begin
      Result := False;
      exit;
   end;

   temp := UpperCase(PathEnvVar);

   if pos(Uppercase(CommonFilesPath) + '\AJAPPS',temp) = 0 then
   begin
      PathEnvVar := PathEnvVar + ';' + CommonFilesPath + '\AJApps';

      if not SetEnvVar('Path',PathEnvVar) then
      begin
         Result := False;
         exit;
      end;
   end;

   Result := True;
end;

// ***********************************************************************
// Take a file path and return the path after updir
// ***********************************************************************
function  FilePathUpDir(fp : string) : string;
begin
   Result := TrimFromLast(fp,'\');
   Result := TrimFromLast(Result,'\');
end;

// ***********************************************************************
// get the "program files" directory
// ***********************************************************************
function  GetProgramFilesDir : string;
const
   CHAR_BUF = 1024;
var
   TempStr : array [0..CHAR_BUF-1] of char;
begin
   GetEnvironmentvariable(PChar('ProgramFiles'),PChar(@TempStr[0]),CHAR_BUF);
   Result := TempStr;
end;

// ***********************************************************************
// strip away the drive letter
// ***********************************************************************
function  FilePathAndName(FN : string) : string; // strips away the drive letter part
begin
   if pos(':',FN) = 2 then
      result := copy(FN,3,length(FN)-2)
   else
      Result := FN;
end;

// ***********************************************************************
// returns the drive letter, without the the path separator
// valid in Windows systems only
// ***********************************************************************
function  GetDriveLetter(FN : string) : string; // returns the drive letter
begin
   	if pos(':',FN) = 2 then
      	result := copy(FN,1,2)
   	else
      	Result := '';
end;

// ***********************************************************************
// Fill a file with constant bytes
// ***********************************************************************
function WriteByteValToFile(var FH: file; Num : integer; val : byte) : Boolean;
var
   i          : integer;
   NumWritten : integer;
begin
   Result := True;
   
   for i := 1 to Num do
   begin
      NumWritten := 0; // lazarus mod
      blockwrite(FH,val,1,NumWritten);
      if NumWritten <> 1 then
      begin
         Result := False;
         Exit;
      end;
   end;
end;

// **************************************************************************
// function: GetIniFilename
// inputs:   None
// outputs:  Filename (AnsiString)
// Effect:   Return a default INI filename based on the application name
// **************************************************************************
function GetIniFilename : string;
var
   TempStr : string;
begin
   TempStr := Application.ExeName;

   TempStr := StringReplace(TempStr,'.EXE','.INI',[rfIgnoreCase]);

   Result := TempStr;
end;

// *****************************************************************************
// Create Directories for a path
// a function to create all the directories in the path
// NOTE: the filename MUST NOT be in the path variable
// NOTE: WINDOWS specific at this time
// *****************************************************************************
function  CreateDirectories(path : string) : Boolean;
var
   	dirs : TStrings;
    dir : string;
    pathonly : string;
    drv : string;
    tempstr : string;
begin
  	tempstr := ExtractFilePath(path);
  	result := false;
    drv := GetDriveLetter(tempstr);
    pathonly := FilePathAndName(tempstr);
    pathonly := copy(pathonly,2,length(pathonly)-1); // remove the first path separator
    dirs := SplitString(PathDelim,pathonly);

    tempstr := drv;
    for dir in dirs do
    begin
       	result := true;
       	tempstr := tempstr + PathDelim + dir;
        CheckDir(tempstr); // check and create if it does not exist
	end;
end;


end.




