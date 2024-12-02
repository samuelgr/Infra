/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2024
 ***********************************************************************************************//**
 * @file Globals.cpp
 *   Implementation of accessors and mutators for global data items.
 *   Intended for miscellaneous data elements with no other suitable place.
 **************************************************************************************************/

#include "Globals.h"

#include <cstdint>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>

#include "TemporaryBuffer.h"

#include "Internal/ApiWindows.h"

namespace Infra
{
  namespace Globals
  {
    /// Name of the product represented by the running binary that contains this code.
    static std::optional<std::wstring_view> productName;

    /// Version of the product represented by the running binary that contains this code.
    static std::optional<SVersionInfo> productVersion;

    HANDLE GetCurrentProcessHandle(void)
    {
      static HANDLE currentProcessHandle = GetCurrentProcess();
      return currentProcessHandle;
    }

    DWORD GetCurrentProcessId(void)
    {
      static DWORD currentProcessId = GetProcessId(GetCurrentProcess());
      return currentProcessId;
    }

    HINSTANCE GetInstanceHandle(void)
    {
      static HINSTANCE instanceHandle;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            GetModuleHandleEx(
                GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
                reinterpret_cast<LPCWSTR>(&GetInstanceHandle),
                &instanceHandle);
          });

      return instanceHandle;
    }

    const SYSTEM_INFO& GetSystemInformation(void)
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

    SVersionInfo GetInfraVersion(void)
    {
      return GitVersionInfoForCurrentProject();
    }

    std::optional<std::wstring_view> GetProductName(void)
    {
      return productName;
    }

    std::optional<SVersionInfo> GetProductVersion(void)
    {
      return productVersion;
    }

    std::wstring_view GetExecutableCompleteFilename(void)
    {
      static std::wstring executableCompleteFilename;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            TemporaryBuffer<wchar_t> buf;
            GetModuleFileName(nullptr, buf.Data(), static_cast<DWORD>(buf.Capacity()));

            executableCompleteFilename.assign(buf.Data());
          });

      return executableCompleteFilename;
    }

    std::wstring_view GetExecutableBaseName(void)
    {
      static std::wstring_view executableBaseName;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            executableBaseName = GetExecutableCompleteFilename();

            const size_t lastBackslashPos = executableBaseName.find_last_of(L"\\");
            if (std::wstring_view::npos != lastBackslashPos)
              executableBaseName.remove_prefix(1 + lastBackslashPos);
          });

      return executableBaseName;
    }

    std::wstring_view GetExecutableDirectoryName(void)
    {
      static std::wstring_view executableDirectoryName;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            executableDirectoryName = GetExecutableCompleteFilename();

            const size_t lastBackslashPos = executableDirectoryName.find_last_of(L"\\");
            if (std::wstring_view::npos != lastBackslashPos)
            {
              executableDirectoryName.remove_suffix(
                  executableDirectoryName.length() - lastBackslashPos);
            }
          });

      return executableDirectoryName;
    }

    std::wstring_view GetThisModuleCompleteFilename(void)
    {
      static std::wstring thisModuleCompleteFilename;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            TemporaryBuffer<wchar_t> buf;
            GetModuleFileName(
                Globals::GetInstanceHandle(), buf.Data(), static_cast<DWORD>(buf.Capacity()));

            thisModuleCompleteFilename.assign(buf.Data());
          });

      return thisModuleCompleteFilename;
    }

    std::wstring_view GetThisModuleBaseName(void)
    {
      static std::wstring_view thisModuleBaseName;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            thisModuleBaseName = GetThisModuleCompleteFilename();

            const size_t lastBackslashPos = thisModuleBaseName.find_last_of(L"\\");
            if (std::wstring_view::npos != lastBackslashPos)
              thisModuleBaseName.remove_prefix(1 + lastBackslashPos);
          });

      return thisModuleBaseName;
    }

    std::wstring_view GetThisModuleDirectoryName(void)
    {
      static std::wstring_view thisModuleDirectoryName;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            thisModuleDirectoryName = GetThisModuleCompleteFilename();

            const size_t lastBackslashPos = thisModuleDirectoryName.find_last_of(L"\\");
            if (std::wstring_view::npos != lastBackslashPos)
            {
              thisModuleDirectoryName.remove_suffix(
                  thisModuleDirectoryName.length() - lastBackslashPos);
            }
          });

      return thisModuleDirectoryName;
    }

    void SetProductInformation(std::wstring_view newProductName, SVersionInfo newProductVersion)
    {
      productName = newProductName;
      productVersion = newProductVersion;
    }
  } // namespace Globals
} // namespace Infra
