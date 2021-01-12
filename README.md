# Large Address Memory Status

 LARGE_ADDRESS  in the EXP file of the EXE will allow the process to have more than 2GB of memory. With 32-bit Windows Server Edition you could get 3GB if it was the machine was booted with a special /3GB switch. With 64-bit Windows both Desktop and Server get almost 4GB without any special switch.

 This project is configured as LARGE_ADDRESS. Its purpose is to show the results of calling GlobalMemoryStatusEx() to see if you really have 4GB of memory. MSDN documents the MEMORYSTATUSEX.ullTotalVirtual member as:

 > The size of the user-mode portion of the virtual address space of the calling process, in bytes. This value depends on the type of process, the type of processor, and the configuration of the operating system. For example, this value is approximately 2 GB for most 32-bit processes on an x86 processor and approximately 3 GB for 32-bit processes that are large address aware running on a system with 4-gigabyte tuning enabled.

https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-globalmemorystatusex

https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/ns-sysinfoapi-memorystatusex

In the below screen capture you see the first line "TotalVirtual" is almost 4GB (FFFE0000h vs FFFFFFFFh).

![screen cap](readme.png)
