#Return::Run, bash
#e::Run, ::{20d04fe0-3aea-1069-a2d8-08002b30309d}, , Max
#+e::Run, *RunAs "tc", , Max
<#Tab::AltTab

; RunOrActivateOrMinimizeProgram
EnvGet, userProfile, USERPROFILE
RunOrActivateOrMinimizeProgram(Program, WorkingDir="", WindowSize="")
{
	SplitPath Program, ExeFile
	Process, Exist, %ExeFile%
	PID = %ErrorLevel%
	if (PID = 0)
	{
		Run, %Program%, %WorkingDir%, %WindowSize%
	}
	else
	{
		SetTitleMatchMode,2
		DetectHiddenWindows, Off
		IfWinActive, ahk_pid %PID%
		WinMinimize, ahk_pid %PID%
		Else
		IfWinExist, ahk_pid %PID%
		WinActivate, ahk_pid %PID%
		Return
	}
}
#x::RunOrActivateOrMinimizeProgram("C:\Program Files\VcXsrv\vcxsrv.exe", UserProfile)
#+f::RunOrActivateOrMinimizeProgram("C:\Program Files (x86)\Mozilla Firefox\firefox.exe", UserProfile)

#IfWinActive ahk_class VcXsrv/x
{
        !Tab::ControlSend, , !{Tab}, VcXsrv Server - Display swint-PC:0.0
        #Tab::ControlSend, , #{Tab}, VcXsrv Server - Display swint-PC:0.0
        #Return::ControlSend, , #{Return}, VcXsrv Server - Display swint-PC:0.0
        #+Return::ControlSend, , #+{Return}, VcXsrv Server - Display swint-PC:0.0
        #^Return::ControlSend, , #^{Return}, VcXsrv Server - Display swint-PC:0.0
        ^Space::ControlSend, , ^{Space}, VcXsrv Server - Display swint-PC:0.0
        #a::ControlSend, , #a, VcXsrv Server - Display swint-PC:0.0
        #s::ControlSend, , #s, VcXsrv Server - Display swint-PC:0.0
        #d::ControlSend, , #d, VcXsrv Server - Display swint-PC:0.0
        #f::ControlSend, , #f, VcXsrv Server - Display swint-PC:0.0
        #g::ControlSend, , #g, VcXsrv Server - Display swint-PC:0.0
        #h::ControlSend, , #h, VcXsrv Server - Display swint-PC:0.0
        #j::ControlSend, , #j, VcXsrv Server - Display swint-PC:0.0
        #k::ControlSend, , #k, VcXsrv Server - Display swint-PC:0.0
        #l::ControlSend, , #l, VcXsrv Server - Display swint-PC:0.0
        #q::ControlSend, , #q, VcXsrv Server - Display swint-PC:0.0
        #w::ControlSend, , #w, VcXsrv Server - Display swint-PC:0.0
        #t::ControlSend, , #t, VcXsrv Server - Display swint-PC:0.0
        #y::ControlSend, , #y, VcXsrv Server - Display swint-PC:0.0
        #u::ControlSend, , #u, VcXsrv Server - Display swint-PC:0.0
        #i::ControlSend, , #i, VcXsrv Server - Display swint-PC:0.0
        #o::ControlSend, , #o, VcXsrv Server - Display swint-PC:0.0
        #p::ControlSend, , #p, VcXsrv Server - Display swint-PC:0.0
        #+p::ControlSend, , #+p, VcXsrv Server - Display swint-PC:0.0
        #^p::ControlSend, , #^p, VcXsrv Server - Display swint-PC:0.0
        #z::ControlSend, , #z, VcXsrv Server - Display swint-PC:0.0
        #c::ControlSend, , #c, VcXsrv Server - Display swint-PC:0.0
        #v::ControlSend, , #v, VcXsrv Server - Display swint-PC:0.0
        #b::ControlSend, , #b, VcXsrv Server - Display swint-PC:0.0
        #n::ControlSend, , #n, VcXsrv Server - Display swint-PC:0.0
        #m::ControlSend, , #m, VcXsrv Server - Display swint-PC:0.0
        #+m::ControlSend, , #+m, VcXsrv Server - Display swint-PC:0.0
        #^m::ControlSend, , #^m, VcXsrv Server - Display swint-PC:0.0
        #0::ControlSend, , #0, VcXsrv Server - Display swint-PC:0.0
        #1::ControlSend, , #1, VcXsrv Server - Display swint-PC:0.0
        #2::ControlSend, , #2, VcXsrv Server - Display swint-PC:0.0
        #3::ControlSend, , #3, VcXsrv Server - Display swint-PC:0.0
        #4::ControlSend, , #4, VcXsrv Server - Display swint-PC:0.0
        #5::ControlSend, , #5, VcXsrv Server - Display swint-PC:0.0
        #6::ControlSend, , #6, VcXsrv Server - Display swint-PC:0.0
        #7::ControlSend, , #7, VcXsrv Server - Display swint-PC:0.0
        #8::ControlSend, , #8, VcXsrv Server - Display swint-PC:0.0
        #9::ControlSend, , #9, VcXsrv Server - Display swint-PC:0.0
        #-::ControlSend, , #-, VcXsrv Server - Display swint-PC:0.0
        #=::ControlSend, , #=, VcXsrv Server - Display swint-PC:0.0
}
