# Microsoft Developer Studio Project File - Name="ole" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=ole - Win32 Bytecode
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ole.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ole.mak" CFG="ole - Win32 Bytecode"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ole - Win32 Bytecode" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "ocamake ole.dsp"
# PROP BASE Rebuild_Opt "-all"
# PROP BASE Target_File "ole.exe"
# PROP BASE Bsc_Name ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake ole.dsp -o olegen.exe"
# PROP Rebuild_Opt "-all"
# PROP Target_File "olegen.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""
# Begin Target

# Name "ole - Win32 Bytecode"

!IF  "$(CFG)" == "ole - Win32 Bytecode"

!ENDIF 

# Begin Source File

SOURCE=.\dllocamole.dll
# End Source File
# Begin Source File

SOURCE=.\ocamole.ml
# End Source File
# Begin Source File

SOURCE=.\olegen.ml
# End Source File
# End Target
# End Project
