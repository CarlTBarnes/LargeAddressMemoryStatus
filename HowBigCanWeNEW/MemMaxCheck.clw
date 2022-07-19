!Find the Maxiumum Possible NEW() by brute force. 
!Alternative would be to build a VM Map but that requires calling VirtualQuery for every address block.

!Helpful to also view memory with VMMAP  https://docs.microsoft.com/en-us/sysinternals/downloads/vmmap

!Original program from https://github.com/CarlTBarnes/LargeAddressMemoryStatus
!Use GlobalMemoryStatusEx() to see if you have 4GB so are Large_Address Aware
!This requires the EXE EXP file to have the LARGE_ADDRESS line
!If operating system is Windows 64-bit then you get 4GB of Virtual Memory
!For 32-Bit Windows it must be booted with the /3GB switch and you get 3GB of Virtual Memory. This tends to rare and should only be used on servers.

!This Cwproj has an EXP file with the below lines
!    NAME 'MemStatus_LargeAA' GUI
!    LARGE_ADDRESS
!------------------------------------------------

    PROGRAM
    INCLUDE 'KeyCodes.CLW'

!GlobalMemoryStatusEx() is a pain having 64-bit intergers. MSDN says 
!     GlobalMemoryStatusEx(LONG LPMEMORYSTATUSEX),BOOL,PASCAL,DLL(1)
MEMORYSTATUSEX_Type   GROUP,TYPE !,PRE()    !typedef struct _MEMORYSTATUSEX {
Length                   UNSIGNED                                  !DWORD dwLength  =SIZE(Group)
MemoryLoad               UNSIGNED                                  !DWORD dwMemoryLoad
TotalPhys                GROUP(INT64).                             !DWORDLONG ullTotalPhys
AvailPhys                GROUP(INT64).                             !DWORDLONG ullAvailPhys
TotalPageFile            GROUP(INT64).                             !DWORDLONG ullTotalPageFile
AvailPageFile            GROUP(INT64).                             !DWORDLONG ullAvailPageFile
TotalVirtual             GROUP(INT64).                             !DWORDLONG ullTotalVirtual
AvailVirtual             GROUP(INT64).                             !DWORDLONG ullAvailVirtual
AvailExtendedVirtual     GROUP(INT64).                             !DWORDLONG ullAvailExtendedVirtual
                      END !} MEMORYSTATUSEX, *LPMEMORYSTATUSEX;
!https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-globalmemorystatusex
!https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/ns-sysinfoapi-memorystatusex

!MSDN says this will be wrong, but I thin the Virtual Memory will be right, so test it out 
MEMORYSTATUS_Type    GROUP,TYPE !,PRE()    !typedef struct _MEMORYSTATUS {
Length                  ULONG                              !DWORD dwLength
MemoryLoad              ULONG                              !DWORD dwMemoryLoad
TotalPhys               ULONG                              !SIZE_T dwTotalPhys
AvailPhys               ULONG                              !SIZE_T dwAvailPhys
TotalPageFile           ULONG                              !SIZE_T dwTotalPageFile
AvailPageFile           ULONG                              !SIZE_T dwAvailPageFile
TotalVirtual            ULONG                              !SIZE_T dwTotalVirtual
AvailVirtual            ULONG                              !SIZE_T dwAvailVirtual
    END !} MEMORYSTATUS, *LPMEMORYSTATUS;


!INT64    GROUP,TYPE  <--- FYI
!lo         ULONG
!hi         LONG
!         END

 
    MAP
MaxMemoryFind           PROCEDURE(ULONG pMaxAvailable)        
MemoryStatusView        PROCEDURE()        
DB          PROCEDURE(STRING DebugMessage)
DBClear     PROCEDURE()     !Clear DebugView Buffer
Hex8        PROCEDURE(LONG LongInt),STRING

        MODULE('RTL')
LenFastClip      PROCEDURE(CONST *STRING Text2Measure),LONG,NAME('Cla$FASTCLIP'),DLL(dll_mode)
ClaFieldNameRTL  PROCEDURE(LONG pFEQ),CSTRING,RAW,NAME('Cla$FIELDNAME'),DLL(dll_mode)
ClaEventNameRTL  PROCEDURE(LONG EventPlusA000h),*CSTRING,RAW,NAME('WslDebug$MsgName'),DLL(dll_mode)
C5LogSetName     PROCEDURE(CONST *CSTRING),NAME('WslDebug$SetLogFile'),DLL(dll_mode)
C5LogPrint       PROCEDURE(STRING),NAME('WslDebug$Print'),DLL(dll_mode)
C5LogPrintEvent  PROCEDURE(),NAME('WslDebug$PrintEvent'),DLL(dll_mode)  !E.g. Event: EVENT:Accepted ?WSLDEBUGPRINT_BTN (10)
        END
      MODULE('api')
        OutputDebugString(*CSTRING cMsg),PASCAL,DLL(1),RAW,NAME('OutputDebugStringA')
        DebugBreak(),PASCAL,DLL(1)  !If running under Debug forces the debugger to popup
        GetLastError(),LONG,PASCAL,DLL(1)     

        GlobalMemoryStatusEx(LONG LPMEMORYSTATUSEX),BOOL,PASCAL,DLL(1)
        GlobalMemoryStatus(LONG LPMEMORYSTATUS),BOOL,PASCAL,DLL(1)      !Not EX so 32-bit

VirtualAlloc  PROCEDURE(*ULONG LPVOID_lpAddress, ULONG dwSize, LONG flAllocationType, LONG flProtect |  
                       ),LONG,RAW,PASCAL,DLL(1) !LPVOID returns  
VirtualFree   PROCEDURE(*ULONG LPVOID_lpAddress, ULONG dwSize, LONG dwFreeType),BOOL,RAW,PASCAL,DLL(1)                       
        
      END
      INCLUDE 'i64.INC'      
    END

MEM_COMMIT      EQUATE(00001000h)       !Allocates memory charges
MEM_RESERVE     EQUATE(00002000h)       !Reserves a range of the process's virtual address space without allocating any actual physical storage in memory or in the paging file on disk.
MEM_RESET       EQUATE(00008000h)       !Reserves a range of the process's virtual address space without allocating any actual physical storage in memory or in the paging file on disk.
  
PAGE_READWRITE  EQUATE(4)               !Enables read-only or read/write access to the committed region of pages
                                        !https://docs.microsoft.com/en-us/windows/win32/memory/memory-protection-constants
MEM_DECOMMIT    EQUATE(00004000h)       !Decommits the specified region of committed pages. After the operation, the pages are in the reserved state.
MEM_RELEASE     EQUATE(00008000h)       !Releases the specified region of pages, or placeholde
                                        !https://docs.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualfree
MB1 EQUATE(1024*1024)

    CODE   
    SYSTEM{PROP:PropVScroll}=1
    MemoryStatusView()
    RETURN
!=============================================================== 
!Simple start high and keep trying NEW() until it works
MaxMemoryFind           PROCEDURE(ULONG pMaxAvailable)
Warn1Time BYTE,STATIC       
NoDisplay BOOL,STATIC       
MaxAvail  ULONG
TestMax   ULONG  
TestMaxMB ULONG  
TestMaxStr STRING(20) 
TestMaxHex STRING(20) 
Increment   ULONG( 1024 * 1024 )  !1 MB
VM_Address  ULONG  
VM_Base     ULONG  
NewStrRef   &STRING
Window WINDOW('Max Memory Find'),AT(,,200,66),GRAY,IMM,SYSTEM,FONT('Segoe UI',10),RESIZE
        PROMPT('Avail Virtual'),AT(9,5),USE(?PROMPT1)
        ENTRY(@n-14),AT(55,4),USE(MaxAvail),SKIP,RIGHT
        PROMPT('Increment'),AT(9,20),USE(?PROMPT2)
        ENTRY(@n-14),AT(55,20),USE(Increment),SKIP,RIGHT,TIP('Increment')
        BUTTON('&Find Max'),AT(5,36),USE(?FindMaxBtn)
        ENTRY(@n-14),AT(55,39),USE(TestMax),SKIP,RIGHT,TIP('TestMax'),READONLY
        STRING(@s20),AT(137,40),USE(TestMaxStr)
        STRING(@s20),AT(137,50),USE(TestMaxHex)
        CHECK('No Display'),AT(5,53),USE(NoDisplay),SKIP,FONT(,9),TIP('Go faster by not doing a DISP' & |
                'LAY each test')
    END
    CODE
    MaxAvail = pMaxAvailable
    OPEN(Window)
    IF ~Warn1Time THEN 
        Warn1Time=1
        DISPLAY
        IF Message('Just opening this window and exiting increases VM usage by 14MB.' & |
                '||To get a true test of NEW() suggest open and close this window once.', |
                'Find Max NEW()',ICON:Asterisk,'&Close This|&Ignore && Continue')=1 THEN 
           POST(EVENT:CloseWindow)
        END            
    END 
    ACCEPT
        CASE ACCEPTED() 
        OF ?FindMaxBtn ; DO FindMaximumRtn 
        END 
    END
    RETURN
    
FindMaximumRtn ROUTINE
    TestMax = MaxAvail 
    TestMaxStr='' ;  TestMaxHex=''
    DISPLAY     
    LOOP
        IF TestMax < Increment THEN
            DISPLAY
            Message('Could not find minimum increment ' & Increment,'Find Max NEW()')   
            TestMax=0
            BREAK
        END 

        TestMax -= Increment
        IF ~NoDisplay THEN DISPLAY.

        NewStrRef &= NEW(STRING(TestMax))
        IF NOT NewStrRef &= NULL THEN         !NEW worked ? 
           TestMaxMB  = INT(TestMax / MB1)
           TestMaxStr = TestMaxMB &' MB'
           TestMaxHex = Hex8(TestMax)
           DISPLAY 
           CASE Message('NEW Worked for ' & TestMax &' bytes, ' & TestMaxMB &' MB', |
                        'Find Max NEW()', ICON:Zoom,'&Close && Keep|&Dispose String'  ) 
           OF 1 ; !Leave it used so memory reduced
           OF 2 ; DISPOSE(NewStrRef)         
           END  
           BREAK 
        END 

!Did not get to work, probably wrong params. Want largest NEW() so simpler to use that
!        VM_Address = 0
!        VM_Base = VirtualAlloc(VM_Address,TestMax,MEM_RESERVE,PAGE_READWRITE)
!        IF VM_Base <>0 THEN BREAK.


    END 
!===============================================================
MemoryStatusView   PROCEDURE
MemQ    QUEUE,PRE(MemQ)
nBytes      DECIMAL(15)
HexB        STRING(18) 
nMB         DECIMAL(11,2)
nGB         DECIMAL(5,2)
Var         STRING(32) 
Desc        STRING(255) 
Tip         STRING(1000) 
       END
TotalVMPointer  LONG       
Window WINDOW('MemMaxCheck based on Memory Status - GlobalMemoryStatusEx'),AT(,,550,270),CENTER,GRAY,IMM,SYSTEM, |
            FONT('Segoe UI',10),RESIZE
        PROMPT('To tell if EXE is Large_Address_Aware call GlobalMemoryStatusEx() to see if 3GB or 4' & |
                'GB of V Memory (> 2GB).'),AT(4,2),USE(?PROMPT1)
        PROMPT('Check IF MEMORYSTATUSEX.TotalVirtual > 7fFFffFFh (2147483647) Then you are LARGE_ADDRESS.'), |
                AT(4,12),USE(?PROMPT2)
        LIST,AT(3,26),FULL,USE(?LIST:MemQ),VSCROLL,FROM(MemQ),FORMAT('60R(2)|M~Bytes~C(0)@n17b@52R(2' & |
                ')|M~Hex Bytes~C(0)@s18@38R(2)|M~MB~C(0)@n12.2b@25R(2)|M~GB~C(0)@n7.2b@90L(2)|M~Vari' & |
                'able~@s32@50L(2)|MP~Description (see Tip)~@s255@')
        BUTTON('Find Max Segment'),AT(431,7),USE(?MaxFindBtn),TIP('Search for Max Segment')
    END

MemGrp       GROUP(MEMORYSTATUSEX_Type).    
M32Grp       GROUP(MEMORYSTATUS_Type).    
DOO     CLASS
LoadMemStatus   PROCEDURE()
AddQ            PROCEDURE(*INT64 Mem, STRING VarName, STRING Desc)
AddQ            PROCEDURE(*ULONG Mem, STRING VarName, STRING Desc)
AddQBlank       PROCEDURE()
        END
MaxAvail    ULONG        
MaxAvailB4  ULONG        
MaxBefore   ULONG        
MaxDiff     DECIMAL(15)
    CODE
    OPEN(WINDOW)
    0{PROP:text}=clip(0{PROP:text}) &' - Clarion Library ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    DOO.LoadMemStatus() 
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow 
        OF EVENT:CloseWindow
        OF EVENT:PreAlertKey
        OF EVENT:AlertKey
        OF EVENT:Timer
        END
        CASE ACCEPTED() 
        OF ?MaxFindBtn 
                        MaxBefore = MaxAvail
                        MaxMemoryFind( MaxAvail )
                        DOO.LoadMemStatus()           
                        MaxDiff = MaxBefore - MaxAvail                        
                        DISPLAY
                        Message('After MaxMemoryFind the Available Virtual Memory' & |
                                '||    Was<9>' & Format(MaxBefore,@n-14) &' <9>'& Hex8(MaxBefore) & |
                                 '|    Now<9>' & Format(MaxAvail,@n-14)   &' <9>'& Hex8(MaxAvail)   & |
                                '|    Diff<9>' & Format(-1 * MaxDiff,@n-14) &' <9>'& Hex8(MaxDiff) & |
                                        '   '& INT((MaxDiff) / MB1) &' MB' & |
                                '','MaxMemoryFind')
        END
        CASE FIELD()
        END
    END
    CLOSE(WINDOW)

DOO.LoadMemStatus  PROCEDURE()
U64     GROUP(INT64).
    CODE
    CLEAR(MemGrp)
    MemGrp.Length = SIZE(MemGrp) 
    IF ~GlobalMemoryStatusEx(ADDRESS(MemGrp)) THEN 
        Message('GlobalMemoryStatusEx() error ' & GetLastError())
    END
    M32Grp.Length = SIZE(M32Grp)     
    IF ~GlobalMemoryStatus(ADDRESS(M32Grp)) THEN 
        Message('32-bit GlobalMemoryStatus() error ' & GetLastError())
    END     
  !  message(MemGrp.TotalPhys.hi &'|' & MemGrp.TotalPhys.lo &'||' & 10000h * 10000h * MemGrp.TotalPhys.hi + MemGrp.TotalPhys.lo ) 
    FREE(MemQ)  
    DOO.AddQ(MemGrp.TotalVirtual,'TotalVirtual ****','Large AA ? IF > 2147483647 (7fFFffFFh) THEN Large Address Aware' & |
                                                                '<13,10><13,10>The size of the user-mode portion of the virtual address space of the calling process, in bytes. <13,10>This value depends on the type of process, the type of processor, and the configuration of the operating system. <13,10>For example, this value is approximately 2 GB for most 32-bit processes on an x86 processor and approximately 3 GB <13,10>for 32-bit processes that are large address aware running on a system with 4-gigabyte tuning enabled.')
    DOO.AddQBlank()
    CLEAR(MemQ) 
    MemQ:nBytes = MemGrp.MemoryLoad
    MemQ:HexB   =  MemQ:nBytes &'%'
    MemQ:Var   = 'MemoryLoad  0-100' 
    MemQ:Tip   = 'A number between 0 and 100 that specifies the approximate percentage of physical memory that is in use (0 indicates no memory use and 100 indicates full memory use).'
    MemQ:Desc  = MemQ:Tip
    ADD(MemQ)
    
    DOO.AddQ(MemGrp.TotalPhys    ,'TotalPhys'    ,'The amount of actual physical memory, in bytes.')
    DOO.AddQ(MemGrp.AvailPhys    ,'AvailPhys'    ,'The amount of physical memory currently available, in bytes. <13,10>This is the amount of physical memory that can be immediately reused without having to write its contents to disk first. <13,10>It is the sum of the size of the standby, free, and zero lists.')
    DOO.AddQ(MemGrp.TotalPageFile,'TotalPageFile','The current committed memory limit for the system or the current process, whichever is smaller, in bytes. <13,10>To get the system-wide committed memory limit, call GetPerformanceInfo.')
    DOO.AddQ(MemGrp.AvailPageFile,'AvailPageFile','The maximum amount of memory the current process can commit, in bytes. <13,10>This value is equal to or smaller than the system-wide available commit value. <13,10>To calculate the system-wide available commit value, call GetPerformanceInfo <13,10>and subtract the value of CommitTotal from the value of CommitLimit.')

    DOO.AddQBlank()
    DOO.AddQ(MemGrp.TotalVirtual,'TotalVirtual ****' ,'Large AA ? The size of the user-mode portion of the virtual address space of the calling process, in bytes. <13,10>This value depends on the type of process, the type of processor, and the configuration of the operating system. <13,10>For example, this value is approximately 2 GB for most 32-bit processes on an x86 processor and approximately 3 GB <13,10>for 32-bit processes that are large address aware running on a system with 4-gigabyte tuning enabled.')
    DOO.AddQ(MemGrp.AvailVirtual,'AvailVirtual'      ,'The amount of unreserved and uncommitted memory currently in the user-mode portion of the virtual address space of the calling process, in bytes.')
    MaxAvail=MemGrp.AvailVirtual.lo

    DOO.AddQBlank()

    i64sub(MemGrp.TotalPhys , MemGrp.AvailPhys, u64) 
    DOO.AddQ(U64,'Used Phy'  ,'Physical memory used =  TotalPhys-AvailPhys')

    i64sub(MemGrp.TotalVirtual , MemGrp.AvailVirtual, u64)     
    DOO.AddQ(U64,'Used Virtual'  ,'Virtual memory used =  TotalVirtual-AvailVirtual')
    
!    DOO.AddQ(MemGrp.AvailExtendedVirtual,'AvailExtendedVirtual','Reserved. This value is always 0.')
    
    DOO.AddQBlank()
    CLEAR(U64) 
    CLEAR(U64.lo,1) 
    DOO.AddQ(U64,'ULONG Maximum 4GB','CLEAR(U64.lo,1) to show Max 4GB')
    U64.lo=7fffFFFFh
    DOO.AddQ(U64,'LONG Maximum 2GB','7fFFffFFh to show Max 2GB')

    DOO.AddQBlank()

    CLEAR(MemQ) 
    MemQ:Var    = '={32}'
    MemQ:Desc   = '={64}' 
    ADD(MemQ)      

    !----------------- Check the 32-bit Only Function, does NOT work to check for 4GB ... now I know MSDN is correct --------------------
    
    CLEAR(MemQ) 
    MemQ:Var    = 'GlobalMemoryStatus() 32-bit API'
    MemQ:Desc   = 'MSDN: On computers with more than 4 GB of memory, the MEMORYSTATUS structure can return incorrect information, reporting a value of –1 to indicate an overflow. <32,10>If your application is at risk for this behavior, use the GlobalMemoryStatusEx function instead of the GlobalMemoryStatus function.' 
    MemQ:Tip    = MemQ:Desc
    ADD(MemQ)      

    DOO.AddQ(M32Grp.TotalPhys,'TotalPhys'        ,'The amount of actual physical memory, in bytes.')
    DOO.AddQ(M32Grp.AvailPhys,'AvailPhys'        ,'The amount of physical memory currently available, in bytes. <13,10>This is the amount of physical memory that can be immediately reused without having to write its contents to disk first. <13,10>It is the sum of the size of the standby, free, and zero lists.')
    DOO.AddQ(M32Grp.TotalPageFile,'TotalPageFile','The current committed memory limit for the system or the current process, whichever is smaller, in bytes. <13,10>To get the system-wide committed memory limit, call GetPerformanceInfo.')
    DOO.AddQ(M32Grp.AvailPageFile,'AvailPageFile','The maximum amount of memory the current process can commit, in bytes. <13,10>This value is equal to or smaller than the system-wide available commit value. <13,10>To calculate the system-wide available commit value, call GetPerformanceInfo <13,10>and subtract the value of CommitTotal from the value of CommitLimit.')

    DOO.AddQBlank()
    DOO.AddQ(M32Grp.TotalVirtual,'TotalVirtual - Large AA ?' ,'The size of the user-mode portion of the virtual address space of the calling process, in bytes. <13,10>This value depends on the type of process, the type of processor, and the configuration of the operating system. <13,10>For example, this value is approximately 2 GB for most 32-bit processes on an x86 processor and approximately 3 GB <13,10>for 32-bit processes that are large address aware running on a system with 4-gigabyte tuning enabled.')
    DOO.AddQ(M32Grp.AvailVirtual,'AvailVirtual'  ,'The amount of unreserved and uncommitted memory currently in the user-mode portion of the virtual address space of the calling process, in bytes.')
    DOO.AddQBlank()
    RETURN 
!-------------------                      
DOO.AddQ  PROCEDURE(*INT64 Mem, STRING VarName, STRING Desc)  !*INT64 MemB, STRING Name, STRING Tip)
B   DECIMAL(15)
HxHi  STRING(8) 
    CODE
    CLEAR(MemQ) 

    i64ToDecimal(B,Mem) 
    MemQ:nBytes = B    

    HxHi = Hex8(Mem.hi) 
    IF HxHi = '00000000' THEN HxHi=''.
    MemQ:HexB   = HxHi &' '& Hex8(Mem.lo)

    MemQ:nMB    = B / (1024*1024)
    MemQ:nGB    = B / (1024*1024*1024)
    MemQ:Var    = VarName 
    MemQ:Desc   = Desc
    MemQ:Tip    = Desc

    ADD(MemQ) 
!-------------------
DOO.AddQ   PROCEDURE(*ULONG Mem, STRING VarName, STRING Desc)
U32as64  GROUP(INT64).
    CODE
    i64Assign(U32as64,Mem)
    DOO.ADDQ(U32as64, VarName, Desc)
!-------------------    
DOO.AddQBlank  PROCEDURE()
    CODE 
    CLEAR(MemQ) 
    ADD(MemQ)   
!===============================
Hex8 PROCEDURE(LONG Lng)!,STRING
LngAdj  LONG,AUTO,STATIC 
L       BYTE,DIM(4),OVER(LngAdj)
Hex     STRING('0123456789ABCDEF'),STATIC
HX      STRING(9),AUTO,STATIC 
  CODE
  LngAdj = BAND(BSHIFT(Lng, -4),0F0F0F0Fh) + 01010101h
  HX[1]=HEX[L[4]] ; HX[3]=HEX[L[3]] ; HX[5]=HEX[L[2]] ; HX[7]=HEX[L[1]]
  LngAdj=BAND(Lng,0F0F0F0Fh)  + 01010101h
  HX[2]=HEX[L[4]] ; HX[4]=HEX[L[3]] ; HX[6]=HEX[L[2]] ; HX[8]=HEX[L[1]] ; HX[9]='h'
  RETURN HX    
!===============================
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('MemStatus: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+1),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage)
  OutputDebugString( sz )
!------------------
DBClear PROCEDURE()
DbgClear CSTRING('DBGVIEWCLEAR')    !Message to Clear the buffer. Must UPPER and first i.e. without a Prefix
    CODE 
    OutputDebugString(DbgClear)     !Cannot have Prefix, must be first .. so call API directly  
