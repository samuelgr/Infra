/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file Message.cpp
 *   Message output implementation.
 **************************************************************************************************/

#include "Core/Message.h"

#include <sal.h>

#include <cstdarg>
#include <cstdio>
#include <mutex>
#include <optional>
#include <string>

#include "ApiWindows.h"

#include "Core/Mutex.h"
#include "Core/ProcessInfo.h"
#include "Core/Strings.h"
#include "Core/TemporaryBuffer.h"

namespace Infra
{
  namespace Message
  {
    std::wstring_view RecommendedLogFilePath(void)
    {
      static std::wstring recommendedLogFilePath;
      static std::once_flag initFlag;

      std::call_once(
          initFlag,
          []() -> void
          {
            Infra::TemporaryString logFilename;

            PWSTR knownFolderPath;
            const HRESULT result =
                SHGetKnownFolderPath(FOLDERID_Desktop, 0, nullptr, &knownFolderPath);

            if (S_OK == result)
            {
              logFilename << knownFolderPath << L'\\';
              CoTaskMemFree(knownFolderPath);
            }

            logFilename << ProcessInfo::GetProductName() << L'_'
                        << ProcessInfo::GetExecutableBaseName() << L'_'
                        << ProcessInfo::GetCurrentProcessId() << L".log";

            recommendedLogFilePath.assign(logFilename);
          });

      return recommendedLogFilePath;
    }

    /// Enumerates all supported modes of outputting messages.
    enum class EOutputMode
    {
      /// Message is output using a debug string, which debuggers will display.
      DebugString,

      /// Message is output to a log file.
      LogFile,

      /// Message is output to the console via `stderr`.
      Console,

      /// Not used as a value, but separates non-interactive output modes from interactive
      /// output modes.
      InteractiveBoundaryValue,

      /// Message is output using a graphical message box.
      GraphicalMessageBox,

      /// Not used as a value. One higher than the maximum possible value in this enumeration.
      UpperBoundValue,
    };

    /// Handle to the log file, if enabled.
    static FILE* logFileHandle = nullptr;

    /// Specifies the minimum severity required to output a message.
    /// Messages below this severity (i.e. above the integer value that represents this
    /// severity) are not output.
    static ESeverity minimumSeverityForOutput = kDefaultMinimumSeverityForOutput;

    /// Checks if the specified output mode is interactive or non-interactive.
    /// @return `true` if the mode is interactive, `false` otherwise.
    static inline bool IsOutputModeInteractive(const EOutputMode outputMode)
    {
      return (outputMode > EOutputMode::InteractiveBoundaryValue);
    }

    /// Checks if the specified severity is forced interactive (i.e. one of the elements that
    /// will always cause a message to be emitted interactively).
    /// @return `true` if the severity is forced interactive, `false` otherwise.
    static inline bool IsSeverityForcedInteractive(const ESeverity severity)
    {
      return (severity < ESeverity::LowerBoundConfigurableValue);
    }

    /// Selects a character to represent each level of severity, for use when outputting
    /// messages.
    /// @param [in] severity Message severity.
    /// @return Character to use to represent it.
    static wchar_t CharacterForSeverity(const ESeverity severity)
    {
      switch (severity)
      {
        case ESeverity::ForcedInteractiveError:
        case ESeverity::Error:
          return L'E';

        case ESeverity::ForcedInteractiveWarning:
        case ESeverity::Warning:
          return L'W';

        case ESeverity::ForcedInteractiveInfo:
        case ESeverity::Info:
          return L'I';

        case ESeverity::Debug:
        case ESeverity::SuperDebug:
          return L'D';

        default:
          return L'?';
      }
    }

    /// Determines the appropriate modes of output based on the current configuration and
    /// message severity.
    /// @param [in] severity Severity of the message for which an output mode is being chosen.
    /// @param [out] selectedOutputModes Filled with the output modes that are selected. Array
    /// should have EOutputMode::UpperBoundValue elements.
    /// @return Number of output modes selected.
    static int DetermineOutputModes(const ESeverity severity, EOutputMode* selectedOutputModes)
    {
      int numOutputModes = 0;

      if (IsSeverityForcedInteractive(severity))
      {
        // If the severity level is forced interactive, then unconditionally enable an
        // interactive output mode. Also potentially output to an attached debugger.

        selectedOutputModes[numOutputModes++] = EOutputMode::GraphicalMessageBox;

        if (IsDebuggerPresent()) selectedOutputModes[numOutputModes++] = EOutputMode::DebugString;
      }
      else if (IsDebuggerPresent())
      {
        // If a debugger is present, #WillOutputMessageOfSeverity will always return `true`.
        // The goal is tn ensure that debug strings are sent for all messages irrespective
        // of severity. For other configured output modes, it is necessary to filter based
        // on severity. Since all messages are being sent to the debugger, using the console
        // is unnecessary.

        selectedOutputModes[numOutputModes++] = EOutputMode::DebugString;

        if (severity <= minimumSeverityForOutput)
        {
          if (IsLogFileEnabled()) selectedOutputModes[numOutputModes++] = EOutputMode::LogFile;
        }
      }
      else
      {
        // Since a debugger is not present, #WillOutputMessageOfSeverity has already
        // validated that the severity of the message justifies outputting it. It is
        // therefore sufficient just to pick appropriate output modes depending on message
        // subsystem configuration. Prefer a log file if enabled, otherwise use the console.
        // Do not use an interactive output mode in this situation.

        if (IsLogFileEnabled())
          selectedOutputModes[numOutputModes++] = EOutputMode::LogFile;
        else
          selectedOutputModes[numOutputModes++] = EOutputMode::Console;
      }

      return numOutputModes;
    }

    /// Outputs the specified message using standard output.
    /// Requires both a severity and a message string.
    /// @param [in] severity Severity of the message.
    /// @param [in] message Message text.
    static inline void OutputInternalUsingConsole(const ESeverity severity, const wchar_t* message)
    {
      fwprintf_s(
          stderr,
          L"%.*s:[%c] %s\n",
          static_cast<int>(ProcessInfo::GetThisModuleBaseName().length()),
          ProcessInfo::GetThisModuleBaseName().data(),
          CharacterForSeverity(severity),
          message);
    }

    /// Outputs the specified message using a debug string.
    /// Requires both a severity and a message string.
    /// @param [in] severity Severity of the message.
    /// @param [in] message Message text.
    static void OutputInternalUsingDebugString(const ESeverity severity, const wchar_t* message)
    {
      OutputDebugString(Strings::Format(
                            L"%.*s:[%c] %s\n",
                            static_cast<int>(ProcessInfo::GetThisModuleBaseName().length()),
                            ProcessInfo::GetThisModuleBaseName().data(),
                            CharacterForSeverity(severity),
                            message)
                            .AsCString());
    }

    /// Outputs the specified message to the log file.
    /// Requires both a severity and a message string.
    /// @param [in] severity Severity of the message.
    /// @param [in] message Message text.
    static void OutputInternalUsingLogFile(const ESeverity severity, const wchar_t* message)
    {
      TemporaryString outputString;

      // First compose the output string stamp.
      // Desired format is "[(current date) (current time)] [(severity)]"
      outputString << L'[';

      TemporaryBuffer<wchar_t> bufferTimestamp;

      if (0 !=
          GetDateFormatEx(
              LOCALE_NAME_USER_DEFAULT,
              0,
              nullptr,
              L"MM'/'dd'/'yyyy",
              bufferTimestamp.Data(),
              bufferTimestamp.Capacity(),
              nullptr))
        outputString << bufferTimestamp.Data();
      else
        outputString << L"(date not available)";

      if (0 !=
          GetTimeFormatEx(
              LOCALE_NAME_USER_DEFAULT,
              0,
              nullptr,
              L"HH':'mm':'ss",
              bufferTimestamp.Data(),
              bufferTimestamp.Capacity()))
        outputString << L' ' << bufferTimestamp.Data();
      else
        outputString << L" (time not available)";

      // Finish up the stamp and append the message itself.
      outputString << L"] [" << CharacterForSeverity(severity) << L"] " << message << L'\n';

      // Write to the log file.
      fputws(outputString.AsCString(), logFileHandle);
      fflush(logFileHandle);
    }

    /// Outputs the specified message using a graphical message box.
    /// Requires both a severity and a message string.
    /// @param [in] severity Severity of the message.
    /// @param [in] message Message text.
    static void OutputInternalUsingMessageBox(const ESeverity severity, const wchar_t* message)
    {
      UINT messageBoxType = MB_SETFOREGROUND;

      switch (severity)
      {
        case ESeverity::ForcedInteractiveError:
        case ESeverity::Error:
          messageBoxType |= MB_ICONERROR;
          break;

        case ESeverity::ForcedInteractiveWarning:
        case ESeverity::Warning:
          messageBoxType |= MB_ICONWARNING;
          break;

        case ESeverity::ForcedInteractiveInfo:
        case ESeverity::Info:
          messageBoxType |= MB_ICONINFORMATION;
          break;

        default:
          messageBoxType |= MB_OK;
          break;
      }

      MessageBox(nullptr, message, ProcessInfo::GetProductName().data(), messageBoxType);
    }

    /// Outputs the specified message.
    /// Requires both a severity and a message string.
    /// Concurrency-safe.
    /// @param [in] severity Severity of the message.
    /// @param [in] message Message text.
    static void OutputInternal(const ESeverity severity, const wchar_t* message)
    {
      // Internally, at least one of the message output functions (for example, graphical
      // message box) use system objects and subsequently free them using functions that might
      // themselves be hooked such that the hook functions produce message outputs. As a result,
      // this function may be called more than once by the same thread, so a non-recursive mutex
      // would quickly lead to deadlock.
      static RecursiveMutex outputGuard;

      EOutputMode outputModes[static_cast<int>(EOutputMode::UpperBoundValue)];
      const int numOutputModes = DetermineOutputModes(severity, outputModes);

      if (numOutputModes > 0)
      {
        std::scoped_lock lock(outputGuard);

        for (int i = 0; i < numOutputModes; ++i)
        {
          switch (outputModes[i])
          {
            case EOutputMode::DebugString:
              OutputInternalUsingDebugString(severity, message);
              break;

            case EOutputMode::LogFile:
              OutputInternalUsingLogFile(severity, message);
              break;

            case EOutputMode::Console:
              OutputInternalUsingConsole(severity, message);
              break;

            case EOutputMode::GraphicalMessageBox:
              OutputInternalUsingMessageBox(severity, message);
              break;

            default:
              break;
          }
        }
      }
    }

    /// Formats and outputs some text of the given severity.
    /// @param [in] severity Severity of the message.
    /// @param [in] format Message string, possibly with format specifiers.
    /// @param [in] args Variable-length list of arguments to be used for any format specifiers
    /// in the message string.
    static void OutputFormattedInternal(
        const ESeverity severity, const wchar_t* format, va_list args)
    {
      TemporaryBuffer<wchar_t> messageBuf;

      vswprintf_s(messageBuf.Data(), messageBuf.Capacity(), format, args);
      OutputInternal(severity, messageBuf.Data());
    }

    void CreateAndEnableLogFile(std::wstring_view logFilename)
    {
      if (false == IsLogFileEnabled())
      {
        // Open the log file.
        if (0 != _wfopen_s(&logFileHandle, logFilename.data(), L"w,ccs=UTF-8"))
        {
          logFileHandle = nullptr;
          OutputFormatted(ESeverity::Error, L"%s - Unable to create log file.", logFilename.data());
        }

        if (nullptr == logFileHandle) return;

        // Output the log file header.
        static constexpr wchar_t kLogHeaderSeparator[] =
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af"
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af"
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af"
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af"
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af"
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af"
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af"
            L"\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af\x23af";

        fwprintf_s(logFileHandle, L"%s\n", kLogHeaderSeparator);

        const std::wstring_view productName = ProcessInfo::GetProductName();
        fwprintf_s(
            logFileHandle,
            L"%.*s Log\n",
            static_cast<int>(productName.length()),
            productName.data());
        fwprintf_s(logFileHandle, L"%s\n", kLogHeaderSeparator);

        const std::optional<ProcessInfo::SVersionInfo> maybeProductVersion =
            ProcessInfo::GetProductVersion();
        if (maybeProductVersion.has_value())
        {
          const std::wstring_view productVersion = maybeProductVersion->string;
          fwprintf_s(
              logFileHandle,
              L"Version:        %.*s\n",
              static_cast<int>(productVersion.length()),
              productVersion.data());
        }
        fwprintf_s(
            logFileHandle,
            L"Module:         %.*s\n",
            static_cast<int>(ProcessInfo::GetThisModuleCompleteFilename().length()),
            ProcessInfo::GetThisModuleCompleteFilename().data());
        fwprintf_s(
            logFileHandle,
            L"Executable:     %.*s\n",
            static_cast<int>(ProcessInfo::GetExecutableCompleteFilename().length()),
            ProcessInfo::GetExecutableCompleteFilename().data());
        fwprintf_s(logFileHandle, L"Process ID:     %d\n", ProcessInfo::GetCurrentProcessId());
        fwprintf_s(logFileHandle, L"%s\n", kLogHeaderSeparator);
        fflush(logFileHandle);
      }
    }

    bool IsLogFileEnabled(void)
    {
      return (nullptr != logFileHandle);
    }

    void Output(const ESeverity severity, const wchar_t* message)
    {
      const DWORD lastError = GetLastError();

      if (false == WillOutputMessageOfSeverity(severity))
      {
        SetLastError(lastError);
        return;
      }

      OutputInternal(severity, message);
      SetLastError(lastError);
    }

    void OutputFormatted(
        const ESeverity severity, _Printf_format_string_ const wchar_t* format, ...)
    {
      const DWORD lastError = GetLastError();

      if (false == WillOutputMessageOfSeverity(severity))
      {
        SetLastError(lastError);
        return;
      }

      va_list args;
      va_start(args, format);

      OutputFormattedInternal(severity, format, args);

      va_end(args);

      SetLastError(lastError);
    }

    void SetMinimumSeverityForOutput(const ESeverity severity)
    {
      if (severity > ESeverity::LowerBoundConfigurableValue) minimumSeverityForOutput = severity;
    }

    bool WillOutputMessageOfSeverity(const ESeverity severity)
    {
      // Messages of all severities are output unconditionally if a debugger is present.
      // #DetermineOutputModes takes care of selecting the appropriate modes, given the
      // message severity.
      if (IsDebuggerPresent()) return true;

      if ((severity < ESeverity::LowerBoundConfigurableValue) ||
          (severity <= minimumSeverityForOutput))
      {
        // Counter-intuitive: severity *values* increase as severity *levels* decrease.
        // This is checking if the actual severity *level* is above (*value* is below) the
        // highest severity *level* that requires output be non-interactive. If so, then
        // there is no requirement that output be non-interactive.
        if (severity < kMaximumSeverityToRequireNonInteractiveOutput) return true;

        // Check all the selected output modes.
        // If any are interactive, then this message is skipped over.
        EOutputMode outputModes[static_cast<int>(EOutputMode::UpperBoundValue)];
        const int numOutputModes = DetermineOutputModes(severity, outputModes);

        for (int i = 0; i < numOutputModes; ++i)
        {
          if (true == IsOutputModeInteractive(outputModes[i])) return false;
        }

        return true;
      }
      else
        return false;
    }
  } // namespace Message
} // namespace Infra
