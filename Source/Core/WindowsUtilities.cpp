/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2026
 ***********************************************************************************************//**
 * @file WindowsUtilities.cpp
 *   Implementations of generally useful miscellaneous Windows utility functions.
 **************************************************************************************************/

#include "Core/WindowsUtilities.h"

#include <optional>
#include <string_view>

#include "ApiWindows.h"

#include "Core/Strings.h"
#include "Core/TemporaryBuffer.h"

namespace Infra
{
  namespace Windows
  {
    void* GetRealApiFunctionAddress(const char* const funcName, void* const funcStaticAddress)
    {
      // List of low-level binary module handles, specified as the result of a call to LoadLibrary
      // with the name of the binary. Each is checked in sequence for the specified function, which
      // is looked up by base name.
      static const HMODULE hmodLowLevelBinaries[] = {LoadLibraryW(L"KernelBase.dll")};

      void* funcAddress = funcStaticAddress;

      for (int i = 0; (funcAddress == funcStaticAddress) && (i < _countof(hmodLowLevelBinaries));
           ++i)
      {
        if (nullptr != hmodLowLevelBinaries[i])
        {
          void* const funcPossibleAddress = SafeGetProcAddress(hmodLowLevelBinaries[i], funcName);
          if (nullptr != funcPossibleAddress) funcAddress = funcPossibleAddress;
        }
      }

      return funcAddress;
    }

    FARPROC SafeGetProcAddress(HMODULE moduleHandle, LPCSTR procName)
    {
      if (nullptr == moduleHandle) return nullptr;

      const size_t moduleBaseAddress = reinterpret_cast<size_t>(moduleHandle);

      const IMAGE_DOS_HEADER* const dosHeader =
          reinterpret_cast<const IMAGE_DOS_HEADER*>(moduleBaseAddress);
      if (IMAGE_DOS_SIGNATURE != dosHeader->e_magic) return nullptr;

      const IMAGE_NT_HEADERS* ntHeaders =
          reinterpret_cast<const IMAGE_NT_HEADERS*>(moduleBaseAddress + dosHeader->e_lfanew);
      if (IMAGE_NT_SIGNATURE != ntHeaders->Signature) return nullptr;

      const DWORD exportDirectoryStartRva =
          ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
      if (0 == exportDirectoryStartRva) return nullptr;

      const DWORD exportDirectoryEndRva = exportDirectoryStartRva +
          ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;

      const IMAGE_EXPORT_DIRECTORY* exportDirectory =
          reinterpret_cast<IMAGE_EXPORT_DIRECTORY*>(moduleBaseAddress + exportDirectoryStartRva);

      const DWORD* exportedFunctions =
          reinterpret_cast<const DWORD*>(moduleBaseAddress + exportDirectory->AddressOfFunctions);

      std::optional<DWORD> foundFunctionIndex = std::nullopt;

      if (reinterpret_cast<size_t>(procName) <= 0x0FFFF)
      {
        // Importing by ordinal.

        const DWORD requestedOrdinal = static_cast<DWORD>(reinterpret_cast<uintptr_t>(procName));

        if (requestedOrdinal < exportDirectory->Base ||
            requestedOrdinal >= exportDirectory->Base + exportDirectory->NumberOfFunctions)
          return nullptr;

        foundFunctionIndex = requestedOrdinal - exportDirectory->Base;
      }
      else
      {
        // Importing by name.

        const DWORD* exportedNames =
            reinterpret_cast<const DWORD*>(moduleBaseAddress + exportDirectory->AddressOfNames);
        const WORD* exportedNameOrdinals = reinterpret_cast<const WORD*>(
            moduleBaseAddress + exportDirectory->AddressOfNameOrdinals);

        for (DWORD i = 0; i < exportDirectory->NumberOfNames; ++i)
        {
          const char* currentName =
              reinterpret_cast<const char*>(moduleBaseAddress + exportedNames[i]);
          if (0 == std::strcmp(procName, currentName))
          {
            foundFunctionIndex = exportedNameOrdinals[i];
            break;
          }
        }
      }

      if (false == foundFunctionIndex.has_value()) return nullptr;

      const DWORD exportFinalRva = exportedFunctions[*foundFunctionIndex];
      if (0 == exportFinalRva) return nullptr;

      const size_t exportFinalAddress = moduleBaseAddress + exportFinalRva;
      const bool isInExportDirectory =
          ((exportFinalRva >= exportDirectoryStartRva) && (exportFinalRva < exportDirectoryEndRva));

      if (true == isInExportDirectory)
      {
        // Export is forwarded.

        const std::string_view exportForwardString =
            reinterpret_cast<const char*>(exportFinalAddress);
        const size_t exportStringSeparatorPos = exportForwardString.find_last_of('.');
        if (std::string_view::npos == exportStringSeparatorPos) return nullptr;

        TemporaryVector<char> exportForwardModuleNameBuf;
        for (size_t i = 0; i < exportStringSeparatorPos; ++i)
          exportForwardModuleNameBuf.PushBack(exportForwardString[i]);
        exportForwardModuleNameBuf.PushBack('\0');

        const char* exportForwardModuleName = exportForwardModuleNameBuf.Data();
        const char* exportForwardProcName =
            exportForwardString.data() + (1 + exportStringSeparatorPos);

        if ('#' == exportForwardProcName[0])
        {
          // Forwarding is by ordinal number, rather than by name.

          size_t exportForwardOrdinal =
              static_cast<size_t>(std::strtoul(&exportForwardProcName[1], nullptr, 10));
          if (exportForwardOrdinal > 0x0FFFF) return nullptr;
          exportForwardProcName = reinterpret_cast<LPCSTR>(exportForwardOrdinal);
        }

        auto exportForwardToModule = GetModuleHandleA(exportForwardModuleName);
        if (nullptr == exportForwardToModule)
        {
          exportForwardModuleNameBuf.PopBack();
          exportForwardModuleNameBuf.PushBack('.');
          exportForwardModuleNameBuf.PushBack('d');
          exportForwardModuleNameBuf.PushBack('l');
          exportForwardModuleNameBuf.PushBack('l');
          exportForwardModuleNameBuf.PushBack('\0');
          exportForwardToModule = GetModuleHandleA(exportForwardModuleName);
        }

        return SafeGetProcAddress(exportForwardToModule, exportForwardProcName);
      }

      return reinterpret_cast<FARPROC>(exportFinalAddress);
    }
  } // namespace Windows
} // namespace Infra
