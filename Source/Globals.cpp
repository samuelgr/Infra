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
#include <string>
#include <string_view>

namespace Infra
{
  namespace Globals
  {
    /// Holds all static data that falls under the global category. Used to make sure that
    /// globals are initialized as early as possible so that values are available during dynamic
    /// initialization. Implemented as a singleton object.
    class GlobalData
    {
    public:

      /// Returns a reference to the singleton instance of this class.
      /// @return Reference to the singleton instance.
      static GlobalData& GetInstance(void)
      {
        static GlobalData globalData;
        return globalData;
      }

      /// Pseudohandle of the current process.
      HANDLE gCurrentProcessHandle;

      /// PID of the current process.
      DWORD gCurrentProcessId;

      /// Holds information about the current system, as retrieved from Windows.
      SYSTEM_INFO gSystemInformation;

      /// Handle of the instance that represents the running form of this code.
      HINSTANCE gInstanceHandle;

    private:

      GlobalData(void)
          : gCurrentProcessHandle(GetCurrentProcess()),
            gCurrentProcessId(GetProcessId(GetCurrentProcess())),
            gSystemInformation(),
            gInstanceHandle(nullptr)
      {
        GetModuleHandleEx(
            GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
            (LPCWSTR)&GlobalData::GetInstance,
            &gInstanceHandle);
        GetNativeSystemInfo(&gSystemInformation);
      }

      GlobalData(const GlobalData& other) = delete;
    };

    HANDLE GetCurrentProcessHandle(void)
    {
      return GlobalData::GetInstance().gCurrentProcessHandle;
    }

    DWORD GetCurrentProcessId(void)
    {
      return GlobalData::GetInstance().gCurrentProcessId;
    }

    HINSTANCE GetInstanceHandle(void)
    {
      return GlobalData::GetInstance().gInstanceHandle;
    }

    const SYSTEM_INFO& GetSystemInformation(void)
    {
      return GlobalData::GetInstance().gSystemInformation;
    }

    SVersionInfo GetVersion(void)
    {
      constexpr uint16_t kVersionStructured[] = {GIT_VERSION_STRUCT};
      static_assert(4 == _countof(kVersionStructured), "Invalid structured version information.");

      return {
          .major = kVersionStructured[0],
          .minor = kVersionStructured[1],
          .patch = kVersionStructured[2],
          .flags = kVersionStructured[3],
          .string = _CRT_WIDE(GIT_VERSION_STRING)};
    }
  } // namespace Globals
} // namespace Infra
