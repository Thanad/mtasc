# Microsoft Developer Studio Project File - Name="ODLL" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=ODLL - Win32 Bytecode
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ODLL.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ODLL.mak" CFG="ODLL - Win32 Bytecode"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ODLL - Win32 Bytecode" (based on "Win32 (x86) External Target")
!MESSAGE "ODLL - Win32 Native code" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "ODLL - Win32 Bytecode"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "ocamake ODLL.dsp"
# PROP BASE Rebuild_Opt "-all"
# PROP BASE Target_File "ODLL.exe"
# PROP BASE Bsc_Name ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake unix.cma config.cmo ODLL.dsp"
# PROP Rebuild_Opt "-all"
# PROP Target_File "ODLL.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "ODLL - Win32 Native code"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "ocamake -opt ODLL.dsp -o ODLL_opt.exe"
# PROP BASE Rebuild_Opt "-all"
# PROP BASE Target_File "ODLL_opt.exe"
# PROP BASE Bsc_Name ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake -opt ODLL.dsp -o ODLL_opt.exe"
# PROP Rebuild_Opt "-all"
# PROP Target_File "ODLL_opt.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "ODLL - Win32 Bytecode"
# Name "ODLL - Win32 Native code"

!IF  "$(CFG)" == "ODLL - Win32 Bytecode"

!ELSEIF  "$(CFG)" == "ODLL - Win32 Native code"

!ENDIF 

# Begin Group "ML Files"

# PROP Default_Filter "ml;mly;mll"
# Begin Source File

SOURCE=.\odll.ml
# End Source File
# Begin Source File

SOURCE=.\odll.mli
# End Source File
# End Group
# Begin Group "MLI Files"

# PROP Default_Filter "mli"
# End Group
# Begin Group "SRC"

# PROP Default_Filter ""
# Begin Source File

SOURCE=C:\Ocaml\src\parsing\asttypes.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\asmcomp\clambda.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\asmcomp\compilenv.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\utils\config.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\bytecomp\emitcode.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\typing\env.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\typing\ident.ml
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\typing\ident.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\bytecomp\instruct.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\bytecomp\lambda.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\parsing\longident.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\typing\path.ml
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\typing\path.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\typing\primitive.mli
# End Source File
# Begin Source File

SOURCE=C:\Ocaml\src\typing\types.mli
# End Source File
# End Group
# End Target
# End Project
