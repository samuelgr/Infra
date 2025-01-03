@echo off
setlocal enabledelayedexpansion

rem +-----------------------------------------------------------------------------------------------
rem | Infra
rem |   Common infrastructure used by other projects.
rem +-----------------------------------------------------------------------------------------------
rem | Authored by Samuel Grossman
rem | Copyright (c) 2016-2025
rem +-----------------------------------------------------------------------------------------------
rem | GitVersionInfo.bat
rem |   Script for extracting version information from Git. Executed automatically on build.
rem +-----------------------------------------------------------------------------------------------

set script_path=%~dp0
set output_lang=%1
set output_dir=%~f2

set output_base_file=GitVersionInfo.generated
if not "%3"=="" set version_build_metadata=%3.
if "%output_dir%"=="" set output_dir=%script_path%

git --version >NUL 2>NUL
if %ERRORLEVEL%==0 (
    set version_is_dirty=0
    for /f "usebackq tokens=1" %%D in (`git diff --shortstat`) do if not "%%~D"=="" set version_is_dirty=1

    for /f "usebackq delims=v tokens=1" %%V in (`git tag --list v[0-9]*.[0-9]*.[0-9]* --merged HEAD`) do set merged_release_ver=%%~V
    if not "!merged_release_ver!"=="" (
        for /f "usebackq delims=.- tokens=1" %%V in (`echo !merged_release_ver!`) do set version_major=%%~V
        for /f "usebackq delims=.- tokens=2" %%V in (`echo !merged_release_ver!`) do set version_minor=%%~V
        for /f "usebackq delims=.- tokens=3" %%V in (`echo !merged_release_ver!`) do set version_patch=%%~V
        for /f "usebackq" %%V in (`git rev-list --count v!merged_release_ver!..HEAD`) do set version_commit_distance=%%~V
    ) else (
        echo No prior version tag could be located. Using distance from initial commit.

        set version_major=0
        set version_minor=0
        set version_patch=0
        for /f "usebackq" %%V in (`git rev-list --count HEAD`) do set version_commit_distance=%%~V

        if "!version_commit_distance!"=="" (
            echo Failed to obtain distance from initial commit.

            set version_commit_distance=0
            set version_build_metadata=unknown.!version_build_metadata!
        )
    )

    if "!version_commit_distance!!version_is_dirty!"=="00" (
        set version_string=!version_major!.!version_minor!.!version_patch!
    ) else (
        for /f "usebackq tokens=1" %%V in (`git describe --abbrev^=8 --always`) do (
            set /a version_patch_plus_one=!version_patch!+1
            set version_string=!version_major!.!version_minor!.!version_patch_plus_one!-dev.!version_commit_distance!.!version_is_dirty!
            
            if not "!version_is_dirty!"=="0" (
                set version_build_metadata=%%~V.dirty.!version_build_metadata!
            ) else (
                set version_build_metadata=%%~V.!version_build_metadata!
            )
        )
    )
) else (
    echo Git is not installed. Unable to determine version information.

    set version_major=0
    set version_minor=0
    set version_patch=0
    set version_commit_distance=0
    set version_is_dirty=0
    set version_string=!version_major!.!version_minor!.!version_patch!
    set version_build_metadata=unknown.!version_build_metadata!
)

if not "%version_build_metadata%"=="" (
    set version_string=%version_string%+%version_build_metadata:~0,-1%
)

set /a version_flags=(%version_commit_distance%*16)+(%version_is_dirty%*1)

if "%version_is_dirty%"=="0" (
    set version_is_dirty_bool=false
) else (
    set version_is_dirty_bool=true
)

if "%output_lang%"=="bat" (
    set output_file=%output_dir%\%output_base_file%.bat

    set define_preamble1=@echo off
    set define_preamble2=rem Git version information
    set define_preamble3=rem Auto-generated by %~n0%~x0

    set define_version_major=set "GIT_VERSION_MAJOR=%version_major%"
    set define_version_minor=set "GIT_VERSION_MINOR=%version_minor%"
    set define_version_patch=set "GIT_VERSION_PATCH=%version_patch%"
    set define_version_flags=set "GIT_VERSION_FLAGS=%version_flags%"
    set define_version_commit_distance=set "GIT_VERSION_COMMIT_DISTANCE=%version_commit_distance%"
    set define_version_is_dirty=set "GIT_VERSION_IS_DIRTY=%version_is_dirty%"
    set define_version_struct=set "GIT_VERSION_STRUCT=%version_major%,%version_minor%,%version_patch%,%version_flags%"
    set define_version_string=set "GIT_VERSION_STRING=%version_string%"
) else if "%output_lang%"=="cpp" (
    set output_file=%output_dir%\%output_base_file%.h

    set define_preamble1=// Git version information
    set define_preamble2=// Auto-generated by %~n0%~x0

    set define_version_major=#define GIT_VERSION_MAJOR %version_major%
    set define_version_minor=#define GIT_VERSION_MINOR %version_minor%
    set define_version_patch=#define GIT_VERSION_PATCH %version_patch%
    set define_version_flags=#define GIT_VERSION_FLAGS %version_flags%
    set define_version_commit_distance=#define GIT_VERSION_COMMIT_DISTANCE %version_commit_distance%
    set define_version_is_dirty=#define GIT_VERSION_IS_DIRTY %version_is_dirty%
    set define_version_struct=#define GIT_VERSION_STRUCT %version_major%,%version_minor%,%version_patch%,%version_flags%
    set define_version_string=#define GIT_VERSION_STRING "%version_string%"
) else if "%output_lang%"=="vsprops" (
    set output_file=%output_dir%\%output_base_file%.props

    set define_preamble1=^<?xml version="1.0" encoding="utf-8"?^>
    set define_preamble2=^<^^!-- Git version information
    set define_preamble3=     Auto-generated by %~n0%~x0 --^>
    set define_preamble4=^<Project ToolsVersion="4.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003"^>
    set define_preamble5=  ^<PropertyGroup^>

    set define_version_major=    ^<GitVersionMajor^>%version_major%^</GitVersionMajor^>
    set define_version_minor=    ^<GitVersionMinor^>%version_minor%^</GitVersionMinor^>
    set define_version_patch=    ^<GitVersionPatch^>%version_patch%^</GitVersionPatch^>
    set define_version_flags=    ^<GitVersionFlags^>%version_flags%^</GitVersionFlags^>
    set define_version_commit_distance=    ^<GitCommitDistance^>%version_commit_distance%^</GitCommitDistance^>
    set define_version_is_dirty=    ^<GitVersionIsDirty^>%version_is_dirty_bool%^</GitVersionIsDirty^>
    set define_version_struct=    ^<GitVersionStruct^>%version_major%,%version_minor%,%version_patch%,%version_flags%^</GitVersionStruct^>
    set define_version_string=    ^<GitVersionString^>%version_string%^</GitVersionString^>

    set define_epilogue1=  ^</PropertyGroup^>
    set define_epilogue2=^</Project^>
) else if "%output_lang%"=="cs" (
    set output_file=%output_dir%\%output_base_file%.cs

    set define_preamble1=// Git version information
    set define_preamble2=// Auto-generated by %~n0%~x0
    set define_preamble3=namespace BuildDefinitions
    set define_preamble4={
    set define_preamble5=  static class GitVersionInfo
    set define_preamble6=  {

    set define_version_major=    public const int GitVersionMajor = %version_major%;
    set define_version_minor=    public const int GitVersionMinor = %version_minor%;
    set define_version_patch=    public const int GitVersionPatch = %version_patch%;
    set define_version_flags=    public const int GitVersionFlags = %version_flags%;
    set define_version_commit_distance=    public const int GitCommitDistance = %version_commit_distance%;
    set define_version_is_dirty=    public const bool GitVersionIsDirty = %version_is_dirty_bool%;
    set define_version_struct=    public const string GitVersionStruct = "%version_major%.%version_minor%.%version_patch%.%version_flags%";
    set define_version_string=    public const string GitVersionString = "%version_string%";

    set define_epilogue1=  }
    set define_epilogue2=}
) else (
  echo No output generated because the requested language "%output_lang%" is unsupported.
  exit /b 1
)

if ""=="%output_file%" exit /b 1

if not exist %output_dir% md %output_dir%
if exist %output_file% del /f %output_file%

echo Writing file %output_file%
echo -----
if not ""=="!define_preamble1!" echo !define_preamble1!
if not ""=="!define_preamble2!" echo !define_preamble2!
if not ""=="!define_preamble3!" echo !define_preamble3!
if not ""=="!define_preamble4!" echo !define_preamble4!
if not ""=="!define_preamble5!" echo !define_preamble5!
if not ""=="!define_preamble6!" echo !define_preamble6!
if not ""=="!define_preamble7!" echo !define_preamble7!
if not ""=="!define_preamble8!" echo !define_preamble8!
echo !define_version_major!
echo !define_version_minor!
echo !define_version_patch!
echo !define_version_flags!
echo !define_version_commit_distance!
echo !define_version_is_dirty!
echo !define_version_struct!
echo !define_version_string!
if not ""=="!define_epilogue1!" echo !define_epilogue1!
if not ""=="!define_epilogue2!" echo !define_epilogue2!
if not ""=="!define_epilogue3!" echo !define_epilogue3!
if not ""=="!define_epilogue4!" echo !define_epilogue4!
if not ""=="!define_epilogue5!" echo !define_epilogue5!
if not ""=="!define_epilogue6!" echo !define_epilogue6!
if not ""=="!define_epilogue7!" echo !define_epilogue7!
if not ""=="!define_epilogue8!" echo !define_epilogue8!
echo -----
if not ""=="!define_preamble1!" echo !define_preamble1! >> %output_file%
if not ""=="!define_preamble2!" echo !define_preamble2! >> %output_file%
if not ""=="!define_preamble3!" echo !define_preamble3! >> %output_file%
if not ""=="!define_preamble4!" echo !define_preamble4! >> %output_file%
if not ""=="!define_preamble5!" echo !define_preamble5! >> %output_file%
if not ""=="!define_preamble6!" echo !define_preamble6! >> %output_file%
if not ""=="!define_preamble7!" echo !define_preamble7! >> %output_file%
if not ""=="!define_preamble8!" echo !define_preamble8! >> %output_file%
echo !define_version_major! >> %output_file%
echo !define_version_minor! >> %output_file%
echo !define_version_patch! >> %output_file%
echo !define_version_flags! >> %output_file%
echo !define_version_commit_distance! >> %output_file%
echo !define_version_is_dirty! >> %output_file%
echo !define_version_struct! >> %output_file%
echo !define_version_string! >> %output_file%
if not ""=="!define_epilogue1!" echo !define_epilogue1! >> %output_file%
if not ""=="!define_epilogue2!" echo !define_epilogue2! >> %output_file%
if not ""=="!define_epilogue3!" echo !define_epilogue3! >> %output_file%
if not ""=="!define_epilogue4!" echo !define_epilogue4! >> %output_file%
if not ""=="!define_epilogue5!" echo !define_epilogue5! >> %output_file%
if not ""=="!define_epilogue6!" echo !define_epilogue6! >> %output_file%
if not ""=="!define_epilogue7!" echo !define_epilogue7! >> %output_file%
if not ""=="!define_epilogue8!" echo !define_epilogue8! >> %output_file%