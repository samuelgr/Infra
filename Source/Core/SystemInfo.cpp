/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2024
 ***********************************************************************************************//**
 * @file SystemInfo.cpp
 *   Implementation of functionality related to getting information about the system on which this
 *   process is running.
 **************************************************************************************************/

#include "Core/SystemInfo.h"

#include <mutex>

#include "ApiWindows.h"

#include "Core/TemporaryBuffer.h"

namespace Infra
{
  namespace SystemInfo
  {
    std::wstring_view GetDnsHostname(void)
    {
      static std::wstring dnsHostname;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            Infra::TemporaryBuffer<wchar_t> buf;
            DWORD bufsize = static_cast<DWORD>(buf.Capacity());

            GetComputerNameEx(ComputerNamePhysicalDnsHostname, buf.Data(), &bufsize);

            dnsHostname.assign(buf.Data());
          });

      return dnsHostname;
    }

    std::wstring_view GetDnsDomain(void)
    {
      static std::wstring dnsDomain;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            Infra::TemporaryBuffer<wchar_t> buf;
            DWORD bufsize = static_cast<DWORD>(buf.Capacity());

            GetComputerNameEx(ComputerNamePhysicalDnsDomain, buf.Data(), &bufsize);

            dnsDomain.assign(buf.Data());
          });

      return dnsDomain;
    }

    std::wstring_view GetDnsFullyQualified(void)
    {
      static std::wstring dnsFullyQualified;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            Infra::TemporaryBuffer<wchar_t> buf;
            DWORD bufsize = static_cast<DWORD>(buf.Capacity());

            GetComputerNameEx(ComputerNamePhysicalDnsFullyQualified, buf.Data(), &bufsize);

            dnsFullyQualified.assign(buf.Data());
          });

      return dnsFullyQualified;
    }

    std::wstring_view GetNetBiosHostname(void)
    {
      static std::wstring netbiosHostname;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            Infra::TemporaryBuffer<wchar_t> buf;
            DWORD bufsize = static_cast<DWORD>(buf.Capacity());

            GetComputerNameEx(ComputerNamePhysicalNetBIOS, buf.Data(), &bufsize);

            netbiosHostname.assign(buf.Data());
          });

      return netbiosHostname;
    }

    const SYSTEM_INFO& GetPlatformAndArchitectureInfo(void)
    {
      static SYSTEM_INFO systemInformation;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            GetNativeSystemInfo(&systemInformation);
          });

      return systemInformation;
    }
  } // namespace SystemInfo
} // namespace Infra
