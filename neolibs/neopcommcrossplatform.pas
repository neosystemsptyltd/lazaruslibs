unit neopcommcrossplatform;

{$mode delphi}
(*********************************************************************
    PComm.pas
     -- PComm Lib unit for Delphi (32 bit version).


    History:   Date       Author         Comment
               5/29/98    Casper         Wrote it.
              12/11/98	  Casper	     Update
              04/27/99    Casper         Update
                                         (add sio_ActXon, sio_ActXoff,
                                              sio_break_ex)
**********************************************************************)

interface

uses Synaser, sysutils;

const
  { baud rate setting }
  B50 = $0;
  B75 = $1;
  B110 = $2;
  B134 = $3;
  B150 = $4;
  B300 = $5;
  B600 = $6;
  B1200 = $7;
  B1800 = $8;
  B2400 = $9;
  B4800 = $A;
  B7200 = $B;
  B9600 = $C;
  B19200 = $D;
  B38400 = $E;
  B57600 = $F;
  B115200 = $10;
  B230400 = $11;
  B460800 = $12;
  B921600 = $13;

  { data bit }
  BIT_5 = $0;
  BIT_6 = $1;
  BIT_7 = $2;
  BIT_8 = $3;

  { stop bit }
  STOP_1 = $0;
  STOP_2 = $4;

  { parity }
  P_EVEN = $18;
  P_ODD  = $8;
  P_SPC  = $38;
  P_MRK  = $28;
  P_NONE = $0;

  { modem control setting }
  C_DTR = $1;
  C_RTS = $2;

  { modem line status }
  S_CTS = $1;
  S_DSR = $2;
  S_RI  = $4;
  S_CD  = $8;

  { error code }
  SIO_OK           = 0;
  SIO_BADPORT      = -1;  { No such port or port not opened }
  SIO_OUTCONTROL   = -2;  { Can't control board }
  SIO_NODATA       = -4;  { No data to read or no buffer to write }
  SIO_OPENFAIL     = -5;   { No such port or port has opened }
  SIO_RTS_BY_HW    = -6;  { Can't set because H/W flowctrl }
  SIO_BADPARM      = -7;  { Bad parameter }
  SIO_WIN32FAIL    = -8;  (* Call win32 function fail, please call }
                             GetLastError to get the error code *)
  SIO_BOARDNOTSUPPORT  = -9;  { Board does not support this function}
  SIO_FAIL         = -10; { PComm function run result fail }
  SIO_ABORT_WRITE  = -11; { Write has blocked, and user abort write }
  SIO_WRITETIMEOUT = -12; { Write timeout has happened }

  { file transfer error code }
  SIOFT_OK           = 0;
  SIOFT_BADPORT      = -1;	{ No such port or port not open }
  SIOFT_TIMEOUT	     = -2;	{ Protocol timeout }
  SIOFT_ABORT        = -3;	{ User key abort }
  SIOFT_FUNC         = -4;	{ Func return abort }
  SIOFT_FOPEN        = -5;	{ Can not open files }
  SIOFT_CANABORT     = -6;	{ Ymodem CAN signal abort }
  SIOFT_PROTOCOL     = -7;	{ Protocol checking error abort }
  SIOFT_SKIP         = -8;	{ Zmodem remote skip this send file }
  SIOFT_LACKRBUF     = -9;	{ Zmodem Recv-Buff size must >= 2K bytes }
  SIOFT_WIN32FAIL    = -10;	(* OS fail }
				  GetLastError to get the error code *)
  SIOFT_BOARDNOTSUPPORT = -11;	{ Board does not support this function}

type

  IrqProc = procedure(port: Longint);
  CallBackProc = function(len: Longint; rlen: Longint; buf: PChar; flen: Longint): Longint;

{Import routine from PComm.dll}
function sio_open(port: Longint): Longint;
function sio_close(port: Longint): Longint;
function sio_ioctl(port, baud, mode: Longint): Longint;
function sio_flowctrl(port, mode: Longint): Longint;
function sio_flush(port, func: Longint): Longint;
function sio_DTR(port, mode: Longint): Longint;
function sio_RTS(port, mode: Longint): Longint;
function sio_lctrl(port, mode: Longint): Longint;
function sio_baud(port, speed: Longint): Longint;
function sio_getch(port: Longint): Longint;
function sio_read(port: Longint; buf: PChar; len: Longint): Longint;
function sio_linput(port: Longint; buf:PChar; len: Longint; term:Longint): Longint;
function sio_putch(port, term: Longint): Longint;
function sio_putb(port: Longint; buf:PChar; len: Longint): Longint;
function sio_write(port: Longint; buf:PChar; len: Longint): Longint;
function sio_putb_x(port: Longint; buf:PChar; len: Longint; tick:Longint): Longint;
function sio_putb_x_ex(port: Longint; buf:PChar; len: Longint; tms:Longint): Longint;
function sio_lstatus(port: Longint): Longint;
function sio_iqueue(port: Longint): Longint;
function sio_oqueue(port: Longint): Longint;
function sio_Tx_hold(port: Longint): Longint;
function sio_getbaud(port: Longint): Longint;
function sio_getmode(port: Longint): Longint;
function sio_getflow(port: Longint): Longint;
function sio_data_status(port: Longint): Longint;
function sio_term_irq(port: Longint; func: IrqProc; code: Byte): Longint;
function sio_cnt_irq(port: Longint; func: IrqProc; count: Longint): Longint;
function sio_modem_irq(port: Longint; func: IrqProc): Longint;
function sio_break_irq(port: Longint; func: IrqProc): Longint;
function sio_Tx_empty_irq(port: Longint; func: IrqProc): Longint;
function sio_break(port, time: Longint): Longint;
function sio_break_ex(port, time: Longint): Longint;
function sio_view(port: Longint; buf: PChar; len: Longint): Longint;
function sio_TxLowWater(port, size: Longint): Longint;
function sio_AbortWrite(port: Longint): Longint;
function sio_AbortRead(port: Longint): Longint;
function sio_SetWriteTimeouts(port, timeouts: Longint): Longint;
function sio_GetWriteTimeouts(port: Longint; var TotalTimeouts:Longint): Longint;
function sio_SetReadTimeouts(port, TotalTimeouts, IntervalTimeouts: Longint): Longint;
function sio_GetReadTimeouts(port: Longint; var TotalTimeouts, IntervalTimeouts: Longint): Longint;
{function sio_ActXon(port: Longint): Longint;
function sio_ActXoff(port: Longint): Longint;
function sio_FtASCIITx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtASCIIRx(port:Longint; fname:PChar; func:CallBackProc; key:Longint; sec:Longint): Longint;
function sio_FtXmodemCheckSumTx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtXmodemCheckSumRx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtXmodemCRCTx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtXmodemCRCRx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtXmodem1KCRCTx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtXmodem1KCRCRx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtYmodemTx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtYmodemRx(port:Longint; var fname:PChar;fno:LongInt;func:CallBackProc; key:Longint): Longint;
function sio_FtZmodemTx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtZmodemRx(port:Longint; var fname:PChar;fno:LongInt;func:CallBackProc; key:Longint): Longint;
function sio_FtKermitTx(port:Longint; fname:PChar; func:CallBackProc; key:Longint): Longint;
function sio_FtKermitRx(port:Longint; var fname:PChar;fno:LongInt;func:CallBackProc; key:Longint): Longint;}


implementation
const
    MAX_COM_PORTS = 255;

    HW_FLOW_CTRL_CTS        = 1; // bit 0: CTS flow control
    HW_FLOW_CTRL_RTS        = 2; // bit 1: RTS flow control
    HW_FLOW_CTRL_TX_XONOFF  = 4; // bit 2: Tx XON/XOFF flow control
    HW_FLOW_CTRL_RX_XONOFF  = 8; // bit 3: Rx XON/XOFF flow control (0 = OFF, 1 = ON)

    FLUSH_INPUT_BUF  = 1;  // 0 : flush input buffer
    FLUSH_OUTPUT_BUF = 2;  // 1 : flush output buffer

type
    TPcommSer = class(TBlockSerial)
    private
        propBaudrate     : integer;
        propStopbits     : integer;
        propDatabits     : integer;
        propParity       : char;
        propHWFlowCtl    : boolean;
        propSWFlowCtl    : boolean;
        propReadTimeout  : Longint;
        propWriteTimeout : Longint;
        ThisPort : integer;

        procedure SetBaudrate(value : integer);
        procedure SetStopbits(value : integer);
        procedure SetDatabits(value : integer);
        procedure SetParity(value : char);
        procedure SetHWFlowCtl(value : boolean);
        procedure SetSWFlowCtl(value : boolean);

        procedure UpdateSettings;
        function comportstr : string;

    public
        property Baudrate     : integer read propBaudrate write SetBaudrate;
        property StopBits     : integer read propStopbits write SetStopbits;
        property DataBits     : integer read propDatabits write SetDatabits;
        property Parity       : char    read propParity   write SetParity;
        property HWFlowCtl    : boolean read propHWFlowCtl write SetHWFlowCtl;
        property SWFlowCtl    : boolean read propHWFlowCtl write SetHWFlowCtl;
        property ReadTimeout  : Longint read propReadTimeout  write propReadTimeout;
        property WriteTimeout : Longint read propWriteTimeout write propWriteTimeout;
    end;

var
    q : integer;
    ser : array [1..MAX_COM_PORTS] of TPcommSer;


// *****************************************************************************
// set the HWFlowCtl
// *****************************************************************************
procedure TPcommSer.SetHWFlowCtl(value : boolean);
begin
    propHWFlowCtl := value;
    UpdateSettings;
end;

// *****************************************************************************
// set the SWFlowCtl
// *****************************************************************************
procedure TPcommSer.SetSWFlowCtl(value : boolean);
begin
    propSWFlowCtl := value;
    UpdateSettings;
end;

// *****************************************************************************
// set the parity
// *****************************************************************************
procedure TPcommSer.SetParity(value : char);
begin
    propParity := value;
    UpdateSettings;
end;

// *****************************************************************************
// set the data bits
// *****************************************************************************
procedure TPcommSer.SetDatabits(value : integer);
begin
    propDatabits := value;
    UpdateSettings;
end;

// *****************************************************************************
// set the stop bits
// *****************************************************************************
procedure TPcommSer.SetStopbits(value : integer);
begin
    propStopbits := value;
    UpdateSettings;
end;

// *****************************************************************************
// Update the port settings
// *****************************************************************************
procedure TPcommSer.UpdateSettings;
begin
    Config(propBaudrate,propDatabits,propParity,propStopbits,propSWFlowCtl,
        propHWFlowCtl);
end;

// *****************************************************************************
// Set the buadrate
// *****************************************************************************
function TPcommSer.comportstr : string;
begin
    Result := 'COM' + SysUtils.IntToStr(ThisPort);
end;

// *****************************************************************************
// Set the buadrate
// *****************************************************************************
procedure TPcommSer.SetBaudrate(value : integer);
begin
    propBaudrate := value;
    UpdateSettings;
end;

{ *****************************************************************************}
function sio_open(port: Longint) : Longint;
begin
    try
        // ser[port] := TPcommSer.Create;
        ser[port].ThisPort  := port;
        ser[port].Connect(ser[port].comportstr);
        ser[port].DataBits:=8;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_close(port: Longint) : LongInt;
begin
    try
        ser[port].CloseSocket;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_ioctl(port, baud, mode: Longint) : Longint;
var
    databits : byte;
    stopbits : byte;
begin
    try
        databits := mode and 3;
        stopbits := mode and 4;

        case databits of
            0: ser[port].DataBits := 5;
            1: ser[port].DataBits := 6;
            2: ser[port].DataBits := 7;
            3: ser[port].DataBits := 8;
        end;

        if stopbits = 0 then
            ser[port].StopBits := 1
        else
            ser[port].StopBits := 2;

        ser[port].Baudrate := Baud;

        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_flowctrl(port, mode: Longint): Longint;
begin
    try
        if ((mode and HW_FLOW_CTRL_CTS) <> 0) or
           ((mode and HW_FLOW_CTRL_RTS) <> 0)
        then
            ser[port].HWFlowCtl := true
        else
            ser[port].HWFlowCtl := false;


        if ((mode and HW_FLOW_CTRL_TX_XONOFF) <> 0) and
           ((mode and HW_FLOW_CTRL_RX_XONOFF) <> 0) then
            ser[port].SWFlowCtl := true
        else
            ser[port].SWFlowCtl := false;

        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
(* Note: func is ignored
*)
function sio_flush(port, func: Longint): Longint;
var
    temp : integer;
begin
    try
        while ser[port].WaitingData <> 0 do
        begin
            temp := ser[port].RecvByte(ser[port].ReadTimeout);
        end;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_DTR(port, mode: Longint): Longint;
begin
    try
        if (mode = 0) then
            ser[port].DTR := false
        else
            ser[port].DTR := true;

        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_RTS(port, mode: Longint): Longint;
begin
    try
        if (mode = 0) then
            ser[port].RTS := false
        else
            ser[port].RTS := true;

        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_lctrl(port, mode: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_baud(port, speed: Longint): Longint;
begin
    try
        ser[port].Baudrate := speed;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_getch(port: Longint): Longint;
var
    temp : byte;
begin
    try
        temp := ser[port].RecvByte(ser[port].ReadTimeout);
        if (ser[port].LastError <> sOK) then
            Result := SIO_FAIL
        else
            Result := Longint(temp);
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_read(port: Longint; buf: PChar; len: Longint): Longint;
var
    rxcnt : longint;
begin
    try
        rxcnt := ser[port].RecvBufferEx(pointer(buf),len,
            ser[port].ReadTimeout);

        if (ser[port].LastError <> sOK) then
            Result := SIO_FAIL
        else
            Result := SIO_OK;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_linput(port: Longint; buf:PChar; len: Longint; term:Longint): Longint;
begin
    Result := SIO_OK;
end;

{ *****************************************************************************}
function sio_putch(port, term: Longint): Longint;
begin
    try
        ser[port].DataBits:=8;
        ser[port].SendByte(term);
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    finally
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_putb(port: Longint; buf:PChar; len: Longint): Longint;
begin
    try
        ser[port].SendBuffer(buf,len);
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    finally
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_write(port: Longint; buf:PChar; len: Longint): Longint;
begin
    try
        ser[port].SendBuffer(buf,len);
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    finally
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_putb_x(port: Longint; buf:PChar; len: Longint; tick:Longint): Longint;
begin
    Result := SIO_OK;
end;

{ *****************************************************************************}
function sio_putb_x_ex(port: Longint; buf:PChar; len: Longint; tms:Longint): Longint;
begin
    Result := SIO_OK;
end;

{ *****************************************************************************}
function sio_lstatus(port: Longint): Longint;
begin
    Result := SIO_OK;
end;

{ *****************************************************************************}
function sio_iqueue(port: Longint): Longint;
begin
    try
        Result := ser[port].WaitingData;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    finally
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_oqueue(port: Longint): Longint;
begin
    try
        Result := ser[port].SendingData;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    finally
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_Tx_hold(port: Longint): Longint;
begin
    Result := SIO_OK;
end;

{ *****************************************************************************}
function sio_getbaud(port: Longint): Longint;
begin
    Result := ser[port].Baudrate;
end;

{ *****************************************************************************}
function sio_getmode(port: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_getflow(port: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_data_status(port: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_term_irq(port: Longint; func: IrqProc; code: Byte): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_cnt_irq(port: Longint; func: IrqProc; count: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_modem_irq(port: Longint; func: IrqProc): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_break_irq(port: Longint; func: IrqProc): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_Tx_empty_irq(port: Longint; func: IrqProc): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_break(port, time: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_break_ex(port, time: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_view(port: Longint; buf: PChar; len: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_TxLowWater(port, size: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_AbortWrite(port: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_AbortRead(port: Longint): Longint;
begin
    Result := SIO_FAIL;
end;

{ *****************************************************************************}
function sio_SetWriteTimeouts(port, timeouts: Longint): Longint;
begin
    try
        ser[port].WriteTimeout := timeouts;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;


{ *****************************************************************************}
function sio_GetWriteTimeouts(port: Longint; var TotalTimeouts:Longint): Longint;
begin
    try
        TotalTimeouts := ser[port].WriteTimeout;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_SetReadTimeouts(port, TotalTimeouts, IntervalTimeouts: Longint): Longint;
begin
    try
        ser[port].ReadTimeout := IntervalTimeouts;
        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
function sio_GetReadTimeouts(port: Longint; var TotalTimeouts, IntervalTimeouts: Longint): Longint;
begin
    try
        TotalTimeouts    := ser[port].ReadTimeout;
        IntervalTimeouts := ser[port].ReadTimeout;

        if (ser[port].LastError = sOK) then
            Result := SIO_OK
        else
            Result := SIO_FAIL;
    except
        Result := SIO_FAIL;
    end;
end;

{ *****************************************************************************}
(*function sio_ActXon;
begin
end;

{ *****************************************************************************}
function sio_ActXoff;
begin
end;

{ *****************************************************************************}
function sio_FtASCIITx;
begin
end;

{ *****************************************************************************}
function sio_FtASCIIRx;
begin
end;

{ *****************************************************************************}
function sio_FtXmodemCheckSumTx;
begin
end;

{ *****************************************************************************}
function sio_FtXmodemCheckSumRx;
begin
end;

{ *****************************************************************************}
function sio_FtXmodemCRCTx;
begin
end;

{ *****************************************************************************}
function sio_FtXmodemCRCRx;
begin
end;

{ *****************************************************************************}
function sio_FtXmodem1KCRCTx;
begin
end;

{ *****************************************************************************}
function sio_FtXmodem1KCRCRx;
begin
end;

{ *****************************************************************************}
function sio_FtYmodemTx;
begin
end;

{ *****************************************************************************}
function sio_FtYmodemRx;
begin
end;

{ *****************************************************************************}
function sio_FtZmodemTx;
begin
end;

{ *****************************************************************************}
function sio_FtZmodemRx;
begin
end;

{ *****************************************************************************}
function sio_FtKermitTx;
begin
end;

{ *****************************************************************************}
function sio_FtKermitRx;
begin
end;*)

{ *****************************************************************************}


{ *****************************************************************************}

initialization
    for q := 1 to MAX_COM_PORTS do
    begin
        ser[q] := TPcommSer.Create;
        ser[q].Baudrate  := 19200;
        ser[q].StopBits  := 0;
        ser[q].DataBits  := 8;
        ser[q].Parity    := 'N';
        ser[q].HWFlowCtl := false;
        ser[q].SWFlowCtl := false;
    end;

{ *****************************************************************************}
finalization
    for q := 1 to MAX_COM_PORTS do
    begin
        if (assigned(ser[q])) then
            ser[q].Free;
    end;
end.

