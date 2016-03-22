unit neotypedef;

{$mode delphi}
interface
uses
   registry, windows, forms, dialogs, sysutils;

const
   // ***************************************************
   CAL_ID_Duotronic_CTIS    = $10000000;  // CTIS TIC Calibration
   CAL_ID_Duotronic_BOOMCON = $10000001;  // duotronic BOOMCON Calibration

 {  ICONS }
   ICON_TABLE         = 286;
   ICON_AXIS          = 302;
   ICON_SNAPSHOT      = 88;
   ICON_OPEN_FOLDER   = 128;
   ICON_CLOSED_FOLDER = 130;
   ICON_VARIABLE      = 248;
   ICON_VARTYPE       = 284;
   ICON_PARAMGRP      = 88;
   
 { GENERAL }
   DEBUG    = 1;

   CHR_TAB    = #9;
   CHR_LF     = #10;
   CHR_RETURN = #13;
   CHR_CR     = #13;

   CR_LF = CHR_CR + CHR_LF;
   LF_CR = CHR_LF + CHR_CR;

   KEY_ESC        = 27;
   KEY_TAB        = 9;
   KEY_CAPSLOCK   = 20;
   KEY_SHIFT      = 16;
   KEY_CTRL       = 17;
   KEY_ALT        = 18;
   KEY_WINDOWS_LT = 91;
   KEY_WINDOWS_RT = 92;
   KEY_MENU       = 93;
   KEY_INS        = 45;
   KEY_DEL        = 46;
   KEY_LEFT       = 37;
   KEY_RIGHT      = 39;
   KEY_UP         = 38;
   KEY_DOWN       = 40;
   KEY_END        = 35;
   KEY_PGDOWN     = 34;
   KEY_PGUP       = 33;
   KEY_HOME       = 36;
   KEY_BCKSPACE   = 8;
   KEY_PAUSE      = 19;
   KEY_SCROLL     = 145;
   KEY_PRTSCR     = 44;
   KEY_ENTER      = 13;

   KEY_F1 = 112;
   KEY_F2 = 113;
   KEY_F3 = 114;
   KEY_F4 = 115;
   KEY_F5 = 116;
   KEY_F6 = 117;
   KEY_F7 = 118;
   KEY_F8 = 119;
   KEY_F9 = 120;
   KEY_F10 = 121;
   KEY_F11 = 122;
   KEY_F12 = 123;

   KEY_MINUS                = 189; // '-' key
   KEY_EQUAL                = 187; // '=' key
   KEY_LEFT_SQUARE_BRACKET  = 219; // '[' key
   KEY_RIGHT_SQUARE_BRACKET = 221; // ']' key

    BIT_0    = $0001;
    BIT_1    = $0002;
    BIT_2    = $0004;
    BIT_3    = $0008;
    BIT_4    = $0010;
    BIT_5    = $0020;
    BIT_6    = $0040;
    BIT_7    = $0080;
    BIT_8    = $0100;
    BIT_9    = $0200;
    BIT_10   = $0400;
    BIT_11   = $0800;
    BIT_12   = $1000;
    BIT_13   = $2000;
    BIT_14   = $4000;
    BIT_15   = $8000;

    NBIT_0    = $FFFFFFFF xor $0001;
    NBIT_1    = $FFFFFFFF xor $0002;
    NBIT_2    = $FFFFFFFF xor $0004;
    NBIT_3    = $FFFFFFFF xor $0008;
    NBIT_4    = $FFFFFFFF xor $0010;
    NBIT_5    = $FFFFFFFF xor $0020;
    NBIT_6    = $FFFFFFFF xor $0040;
    NBIT_7    = $FFFFFFFF xor $0080;
    NBIT_8    = $FFFFFFFF xor $0100;
    NBIT_9    = $FFFFFFFF xor $0200;
    NBIT_10   = $FFFFFFFF xor $0400;
    NBIT_11   = $FFFFFFFF xor $0800;
    NBIT_12   = $FFFFFFFF xor $1000;
    NBIT_13   = $FFFFFFFF xor $2000;
    NBIT_14   = $FFFFFFFF xor $4000;
    NBIT_15   = $FFFFFFFF xor $8000;

    LOAD_OK               = 0;
    LOAD_CANNOT_OPEN_FILE = -1;
    LOAD_TOO_MANY_LINES   = -2;
    LOAD_DATA_PROBLEM     = -3;
    LOAD_GENERAL_ERROR    = -4;

    SAVE_OK                = 0;
    SAVE_CANNOT_SAVE_FILE  = -1;
    SAVE_GENERAL_ERROR     = -2;
    
    COMMENT_STR      = '#REM ';
    TABLENAME_STR    = '#TABLE ';
    FILENAME_STR     = '#FILENAME ';
    TYPE_STR         = '#TYPE ';
    XAXIS_STR        = '#XAXIS ';
    YAXIS_STR        = '#YAXIS ';
    DATA_STR         = '#DATA ';
    AXISNAME_STR     = '#AXISNAME ';
    IDXVARNAME_STR   = '#IDXVARNAME ';
    AXISLENGTH_STR   = '#AXISLENGTH ';
    LENGTH_STR       = '#LENGTH ';
    AXISFILENAME_STR = '#AXISFN ';
    PROJECT_STR      = '#PROJECT ';

    PASSWORD_STR     = 'Passwords';
    USER_STR         = 'Users';
    OPTION_STR       = 'Options';
    EXPIREDATE_STR   = 'ExpiryDate';
    STARTDATE_STR    = 'StartDate';
    GEN_STR          = 'General';

    DEF_TYPES_FILENAME       = 'types.dat';
    DEF_ENGUNITS_FILENAME    = 'units.dat';
    DEF_ENGPREFIX_FILENAME   = 'prefix.dat';
    DEF_PREFIXDESC_FILENAME  = 'prefixDesc.dat';
    DEF_EMBSIZE_FILENAME     = 'embSize.dat';
    DEF_CONVMETHOD_FILENAME  = 'convMethod.dat';

    TYPES_FILENAME        = 'TypesFile';
    ENGUNITS_FILENAME     = 'UnitsFile';
    ENGPREFIX_FILENAME    = 'PrefixFile';
    PREFIXDESC_FILENAME   = 'PrefixDescFile';
    EMBSIZE_FILENAME      = 'EmbSizeFile';
    CONVMETHOD_FILENAME   = 'ConvMethod';
    PARAM_GRP_REPORT_FILE = 'ParamGroup.Rpt';

    DEF_NEW_CALIB_PATH   = 'DefNewCalibPath';
    DEF_NEW_CALIB        = 'Calibrations';

    DEF_NEW_SNAPS_PATH   = 'DefNewSnapsPath';
    DEF_NEW_SNAPS        = 'Snapshots';

    DEF_ACTUATOR_FILE    = 'Actuators.ini';
    LAST_PARAMGRP_FILE   = 'ParamGroupFile';
    LAST_SNAPSHOT_FILE   = 'SnapShotFile';

    DATABASE_EXTS = 'DataBaseTypes';
    DATABASE_EXT  = 'DefDataBaseType';
    DEF_DATABASE_EXTS = 'Data files (*.dat)|*.DAT|All files (*.*)|*.*';
    DEF_DATABASE_EXT = 'DAT';

    mrAppend    = $100000;
    mrOverwrite = $100001;

    DEF_AXIS_PATH      = 'AxisDefinitions';
    DEF_AXIS_PATH_NAME = 'Calibrations\AxisDefinitions';

    DEF_TABLE_PATH      = 'TableDefinitions';
    DEF_TABLE_PATH_NAME = 'Calibrations\TableDefitions';

    DEF_CTRLVARSLIST_PATH      = 'Ctrlvarslist';
    DEF_CTRLVARSLIST_PATH_NAME = 'ControlVars';

    DEF_VALUETYPELIST_PATH      = 'ValueTypeList';
    DEF_VALUETYPELISt_PATH_NAME = 'Calibrations';

    DEF_HELP_PATH        = 'HelpPath';
    DEF_HELP_PATH_NAME   = 'Help';

    LAST_LISTFILE = 'LastListFile';
    DEF_LISTFILE  = 'anyfile.lst';

    LAST_CSF_FILE = 'LastCSFFile';
    DEF_CSF_FILE = 'default.csf';

    LAST_COMPILER_LINKER = 'CompilerLinker';
    DEF_COMPILER_LINKER  = 0;

    DEF_CSF_PATH         = 'CSFPath';
    DEF_CSF_PATH_NAME    = 'CompilerSpecifics';

    LAST_COMPSPECFILE = 'LastCompSpec';
    LAST_CAL_FILE     = 'LastCalibFile';

    DEF_TABLECAT_FILE  = 'data\TableCategories.tre';

    LAST_DOWNLOAD_FILE      = 'LastDownloadFile';
    DEF_LAST_DOWNLOAD_FILE  = 'download.bin';

    LAST_CAL_IMAGE    = 'LastCalImage';
    LAST_CAL_IMAGE_FN = 'DFT400.IMG';

    LAST_LICENSE_FILE = 'LicenseFile';
    DEF_LICENSE_FILE  = 'c:\windows\default.lic';

    LAST_STPROG_FILE = 'LastSTProgFile';

    KWP2000_VW    = 'KWP2000Comms_VW';
    LAST_LOG_FILE = 'LastLogFile';
    DEF_LOG_FILE  = 'Default.log';

    LAST_DESKTOP_FILE = 'LastDesktopFile';

    LAST_INPUTHEX_FILE  = 'LastInputHexFile';
    LAST_OUTPUTHEX_FILE = 'LastOutputHexFile';
    LAST_IMAGE_FILE     = 'LastImageFile';

    LAST_FLASHIMAGE_FILE = 'LastFlashImageFile';
    LAST_RAMIMAGE_FILE   = 'LastRamImageFile';
    LAST_ASMCAL_FILE     = 'LastAsmCalibrationFile';

    DEF_PARAMGRP_FILE = 'ParameterGroups';
    USE_CUST_FONTS    = 'UseCustFonts';
    GNU_EXEC_FILE     = 'GnuExecFile';
    DEF_GNU_EXEC_FILE = 'c:\Progra~1\Gnu\Octave\GnuPlot\wgnuplot.exe';

    TYPES_INTEGRITY_RPT = 'Integrity.rpt';

    // *********************************************************************
    // generic Application exit codes
    APPL_EXIT_CODE_SUCCESS  = 0; // success
    APPL_EXIT_CODE_ERRORS   = 1; // error occured
    APPL_EXIT_CODE_WARNINGS = 2; // warnings occurred
    APPL_EXIT_CODE_ABORT    = 3; // program was aborted by user

    // *********************************************************************
    ftHEXFile = 1;
    ftBINFile = 2;

    FreezeValStr    = '_FreezeVal';
    ValStr          = '_Val';
    FreezeFlagStr   = '_bFreezeFlag';

    // return codes from embedded C167 programming system
    C167ISP_SUCCESS                   = 0;
    C167ISP_SETUP_PROBLEM             = 1;
    C167ISP_NO_BOOTRESPONSE           = 2;
    C167ISP_SECONDARY_NORESP          = 3;
    C167ISP_SECONDARY_INCORRECT_RESP  = 4;
    C167ISP_ERASE_FAIL                = 5;
    C167ISP_PROG_FAIL                 = 6;

    // return message codes for any comms messages in embedded systems
    MSG_ACTION_OK      = 0;
    MSG_RESP_RECEIVED  = 1;
    MSG_RESP_TIMEOUT   = 2;
    MSG_RESP_WAITING   = 3;

    // RAM Test results codes - see TRamTestResult
    // see also RAMTestDesc
    RAM_TEST_FIRST  = 0;
    RAM_TEST_PASSED = 0;
    RAM_TEST_FAILED = 1;
    RAM_TEST_LAST   = 1;
    RAM_TEST_BUSY   = 2;

    // standard Help codes from $6000 onwards?
    STD_HELP_BASE = $6000;

    STD_HELP_EDIT_DOWNLOAD_STRATEGY   = STD_HELP_BASE + 1;
    STD_HELP_COMMUNICATION_SETUP      = STD_HELP_BASE + 2;
    STD_HELP_DOWNLOAD_STRATEGY        = STD_HELP_BASE + 3;
    STD_HELP_PATH_CHOOSER             = STD_HELP_BASE + 4;

type
    TCharFile = file of char;

    TUnit = (utmm, utInches, utPercentage);

    // *********************************************************************
    // buffer types
    T512KBuf    = array [0..$7FFFF] of byte;
    T512KBufPtr = ^T512KBuf;

    T256KBuf    = array [0..$3FFFF] of byte;
    T256KBufPtr = ^T256KBuf;

    T1MBBuf     = array [0..$FFFFF] of Byte;
    T1MBBufPtr  = ^T1MBBuf;

    T4MBBuf     = array [0..$3FFFFF] of Byte;
    T4MBBufPtr  = ^T4MBBuf;

    TAddrPtr = cardinal;   // a representation of a 32 bit embedded address

    // *********************************************************************
    BytePtr      = ^Byte;
    TGenProcPtr  = procedure of object;
    THelpProc    = procedure of object;

    TCharArray   = array [0..$FFFFFF] of PChar;
    PCharArray   = ^PChar;

    TOpenRealArray = array of word; //real;
    POpenRealArray = ^TOpenRealArray;

    File_of_byte = File of byte;

    Str8         = String[8];
    Str80        = String[80];
    Str40        = String[40];
    Str20        = String[20];
    TString40    = String[40];
    Str128       = String[128];
    Str256       = String[255];

    TStrPtr = ^string;

    TDTRState = (DTROff, DTROn);
    TRTSState = (RTSOff, RTSOn);


    TSwapL = packed record
      case Byte of
       1 : (
              LongVal : Cardinal;
           );
       2 : (
              Bytes : array [1..4] of byte;
           );
      end;

    TOption = record
       Tag  : Integer;
       Desc : string;
       ID   : integer;
    end;

    TCheckItem = record
       ID      : integer;
       Desc    : string;
       Checked : boolean;
    end;

    TRamTestResult = packed record // generic ram test result structure
                        wStatus   : word;      // See RAM test result code (RAM_TEST_PASSED etc)
                        pFailAddr : cardinal;
                        wWriteVal : word;
                        wReadVal  : word;
                     end;

    TPCFTime = packed record // generic time structure to interface with embedded systems
                   bMins    : byte;
                   bHours   : byte;
                   bSeconds : byte;
                   bFrac    : byte;
               end;

    TGPSData = packed record
                   Latitude  : array [1..4] of byte;
                   Longitude : array [1..4] of byte;
                   bSpeed    : byte;
                   bCourse   : byte;
                   bQuality  : byte;
                   BSpare1   : byte; // placeholder for even size
               end;
    TC167ProgRetCode = word;
    TMessageCode     = word;

    TFlash28F128Info = packed record
                             wManufacturerCode : word;
                             wDeviceCode       : word;
                       end;

    TBellMemTestResult = packed record
                            wCode      : word;
                            ulLastAddr : cardinal;
                            wLastData  : word;

                            st28F128Info : TFlash28F128Info;
                         end;

type
    TLargeByteArray = array[0..$FFFFFFF] of byte;
    PLargeByteArray = ^TLargeByteArray;

    T64KByteArray = array[0..$FFFF] of byte;
    P64KByteArray = ^T64KByteArray;

    TVarElement = record
      case integer of
         0 : (vByte     : byte);
         1 : (vChar     : char);
         2 : (vInteger  : integer);
         3 : (vWord     : word);
         4 : (vCardinal : Cardinal);
         5 : (vSmallint : SmallInt);
         6 : (vShortint : ShortInt);
         7 : (vPointer  : Pointer);
    end;

const
    RamTestDesc : array [RAM_TEST_FIRST .. RAM_TEST_LAST] of str80 =
    ( 'RAM Test passed',
      'RAM Test Failed'
    );

var
  AppPath : string;

procedure gen_init;
function  GetDefSearchPaths : string;
procedure ShowTBIL;
function  GetHighestByte(x : cardinal) : byte;
function  GetHiByte(x : cardinal) : byte;
function  GetMidByte(x : cardinal) : byte;
function  GetLoByte(x : cardinal) : byte;

function GetIntAtAddr(x : integer; ofs : integer) : integer;
function GetWordAtAddr(x : integer; ofs : integer) : word;
function GetRealAtAddr(x : integer; ofs : integer) : Real;
function PCFToStr(Time : TPCFTime) : string;

implementation
uses
   neofileutils;
   
// ***********************************************************
// Show to be implimented later ....
// ***********************************************************
procedure ShowTBIL;
begin
  MessageDlg('Software under development. This function is to be implemented later. '+
             'Thank you for your patience.',mtInformation,[mbOK],0);
end;

// ***********************************************************
// general init procedure
// not to be used in future!
// ***********************************************************
procedure gen_init;
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;

  AppPath := '';
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\App Paths\dft400.exe', False)
    then
       if DEBUG = 1 then
          Reg.WriteString('Path','c:\projects\P009\software\windows\util');
       AppPath := Reg.ReadString('Path');
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

// ***********************************************************
// return the path list
// ***********************************************************
function GetDefSearchPaths : string;
begin
   GetDefSearchPaths := GetApplicationPath(Application);
end;

// ***********************************************************
// Routines to access a 24 bit address hi, med and lobyte
// ***********************************************************
function  GetHighestByte(x : cardinal) : byte;
var
   Temp : TSwapl;
begin
   Temp := TSwapl(x);
   GetHighestByte := Temp.Bytes[4];
end;

function GetHiByte(x : cardinal) : byte;
var
   Temp : TSwapl;
begin
   Temp := TSwapl(x);
   GetHiByte := Temp.Bytes[3];
end;

function GetMidByte(x : cardinal) : byte;
var
   Temp : TSwapl;
begin
   Temp := TSwapl(x);
   GetMidByte := Temp.Bytes[2];
end;

function GetLoByte(x : cardinal) : byte;
var
   Temp : TSwapl;
begin
   Temp := TSwapl(x);
   GetLoByte := Temp.Bytes[1];
end;

// ************************************************************
// Get the integer at address and offset
// ************************************************************
function IntToPtr(x : integer) : Pointer;
begin
    {$ifdef FPC}
    // code for freepascal (assuming lazarus)
    Result := Pointer(Ptr(0,x));
    {$else}
    // code for other (assuming delphi or others)
    Result := Ptr(x);
    {$endif}
end;

// ************************************************************
// Get the integer at address and offset
// ************************************************************
function GetIntAtAddr(x : integer; ofs : integer) : integer;
var
   Address : ^integer;
begin
   Address := IntToPtr(x + (sizeof(integer) * ofs));

   Result := Address^;
end;

// ************************************************************
// Get the word at address and offset
// ************************************************************
function GetWordAtAddr(x : integer; ofs : integer) : word;
var
   Address : ^word;
begin
   Address := IntToPtr(x + (sizeof(word) * ofs));

   Result := Address^;
end;

// ************************************************************
// Get the real at address and offset
// ************************************************************
function GetRealAtAddr(x : integer; ofs : integer) : Real;
var
   Address : ^Real;
   Temp    : real;
begin
   Address := IntToPtr(x + (sizeof(real) * ofs));

   Temp   := Address^;
   Result := temp;
end;

// ************************************************************
// PCF time to string
// ************************************************************
function PCFToStr(Time : TPCFTime) : string;
begin
   with Time do
     Result := Format('%2d:%2d:%2d.%2d',[bMins,bHours,bSeconds,bFrac]);
end;

end.

