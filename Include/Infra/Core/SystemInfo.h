/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2024
 ***********************************************************************************************//**
 * @file SystemInfo.h
 *   Declaration of functionality related to getting information about the system on which this
 *   process is running.
 **************************************************************************************************/

#pragma once

#include <string>
#include <string_view>

#include "ApiWindows.h"
#include "Strings.h"

namespace Infra
{
  namespace SystemInfo
  {
    /// Retrieves and returns the DNS hostname of the machine on which the currently-running
    /// executable is running. Does not include the domain name, just the hostname.
    std::wstring_view GetDnsHostname(void);

    /// Retrieves and returns the DNS domain of the machine on which the currently-running
    /// executable is running. Does not include the hostname, just the domain.
    std::wstring_view GetDnsDomain(void);

    /// Retrieves and returns the fully-qualified DNS name of the machine on which the
    /// currently-running executable is running. Includes both hostname and domain.
    std::wstring_view GetDnsFullyQualified(void);

    /// Retrieves and returns the NetBIOS hostname of the machine on which the currently-running
    /// executable is running.
    std::wstring_view GetNetBiosHostname(void);

    /// Retrieves platform and architectural information on the current system.
    /// @return Reference to a read-only structure containing system platform and architectural
    /// information.
    const SYSTEM_INFO& GetPlatformAndArchitectureInfo(void);
  } // namespace SystemInfo
} // namespace Infra
