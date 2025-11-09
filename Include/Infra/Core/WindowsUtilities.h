/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file WindowsUtilities.h
 *   Declarations of generally useful miscellaneous Windows utility functions.
 **************************************************************************************************/

#pragma once

#include "ApiWindows.h"

namespace Infra
{
  namespace Windows
  {
    /// Retrieves the proper address of a Windows API function. Many Windows API functions have been
    /// moved to lower-level binaries. See
    /// https://docs.microsoft.com/en-us/windows/win32/win7appqual/new-low-level-binaries for more
    /// information. If possible, use the address in the lower-level binary as the original
    /// function, otherwise just use the static address.
    /// @param [in] funcName API function name.
    /// @param [in] funcStaticAddress Static address of the function.
    /// @return Recommended address to use for the Windows API function, which could be the same as
    /// the static address.
    void* GetRealApiFunctionAddress(const char* const funcName, void* const funcStaticAddress);

    /// Drop-in replacement for the `GetProcAddress` Windows API function, used for safe retrieval
    /// of procedure addresses from module handles. This version is not subject to any of the
    /// changes Windows might normally make to the addressses, such as by allowing interception by a
    /// compatibility layer.
    FARPROC SafeGetProcAddress(HMODULE moduleHandle, LPCSTR procName);
  } // namespace Windows
} // namespace Infra
