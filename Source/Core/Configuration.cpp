/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2024
 ***********************************************************************************************//**
 * @file Configuration.cpp
 *   Implementation of configuration file functionality.
 **************************************************************************************************/

#include "Core/Configuration.h"

#include <algorithm>
#include <climits>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "Core/DebugAssert.h"
#include "Core/Strings.h"
#include "Core/TemporaryBuffer.h"

namespace Infra
{
  namespace Configuration
  {
    /// Enumerates all possible classifications of configuration file lines.
    /// Used during parsing to classify each line encountered.
    enum class ELineClassification
    {
      /// Line could not be parsed.
      Error,

      /// Line should be ignored, either because it is just whitespace or because it is a
      /// comment.
      Ignore,

      /// Line begins a section, whose name appears in square brackets.
      Section,

      /// Line is a value within the current section and so should be parsed.
      Value,
    };

    /// Wrapper around a standard file handle.
    /// Attempts to open the specified file on construction and close it on destruction.
    struct FileHandle
    {
      FILE* fileHandle = nullptr;

      inline FileHandle(const wchar_t* filename, const wchar_t* mode)
      {
        _wfopen_s(&fileHandle, filename, mode);
      }

      FileHandle(const FileHandle& other) = delete;

      inline ~FileHandle(void)
      {
        if (nullptr != fileHandle) fclose(fileHandle);
      }

      inline operator FILE*(void) const
      {
        return fileHandle;
      }

      inline FILE** operator&(void)
      {
        return &fileHandle;
      }

      inline bool IsEndOfInput(void)
      {
        return feof(fileHandle);
      }

      inline bool IsError(void)
      {
        return ferror(fileHandle);
      }
    };

    /// Wrapper that emulates a file handle but for in-memory buffers.
    struct MemoryBufferHandle
    {
      std::wstring_view remainingBuffer;

      inline MemoryBufferHandle(std::wstring_view configBuffer) : remainingBuffer(configBuffer) {}

      inline bool IsEndOfInput(void)
      {
        return remainingBuffer.empty();
      }

      inline bool IsError(void)
      {
        return false;
      }
    };

    /// Determines and returns the canonical single-valued type for each possible multi-valued type
    /// enumerator.
    /// @param [in] valueType Type for which a canonical single-valued enumerator is desired.
    /// @return Canonical single-valued enumerator, which might be the same as the input parameter.
    static EValueType CanonicalSingleValueType(EValueType valueType)
    {
      switch (valueType)
      {
        case EValueType::IntegerMultiValue:
          return EValueType::Integer;
        case EValueType::BooleanMultiValue:
          return EValueType::Boolean;
        case EValueType::StringMultiValue:
          return EValueType::String;
        default:
          return valueType;
      }
    }

    /// Tests if the supplied character is allowed as a configuration setting name (the part
    /// before the '=' sign in the configuration file).
    /// @param [in] charToTest Character to test.
    /// @return `true` if so, `false` if not.
    static bool IsAllowedNameCharacter(wchar_t charToTest)
    {
      switch (charToTest)
      {
        case L'.':
        case L'-':
        case L'_':
          return true;

        default:
          return (iswalnum(charToTest) ? true : false);
      }
    }

    /// Tests if the supplied character is allowed as a section name (appears between square
    /// brackets in the configuration file).
    /// @param [in] charToTest Character to test.
    /// @return `true` if so, `false` if not.
    static bool IsAllowedSectionCharacter(wchar_t charToTest)
    {
      switch (charToTest)
      {
        case L',':
        case L'.':
        case L';':
        case L':':
        case L'\'':
        case L'\\':
        case L'{':
        case L'}':
        case L'-':
        case L'_':
        case L' ':
        case L'+':
        case L'=':
        case L'!':
        case L'@':
        case L'#':
        case L'$':
        case L'%':
        case L'^':
        case L'&':
        case L'(':
        case L')':
          return true;

        default:
          return (iswalnum(charToTest) ? true : false);
      }
    }

    /// Tests if the supplied character is allowed as a configuration setting value (the part
    /// after the '=' sign in the configuration file).
    /// @param [in] charToTest Character to test.
    /// @return `true` if so, `false` if not.
    static bool IsAllowedValueCharacter(wchar_t charToTest)
    {
      switch (charToTest)
      {
        case L',':
        case L'.':
        case L';':
        case L':':
        case L'\'':
        case L'\\':
        case L'{':
        case L'[':
        case L'}':
        case L']':
        case L'-':
        case L'_':
        case L' ':
        case L'+':
        case L'=':
        case L'!':
        case L'@':
        case L'#':
        case L'$':
        case L'%':
        case L'^':
        case L'&':
        case L'(':
        case L')':
        case L'*':
        case L'?':
          return true;

        default:
          return (iswalnum(charToTest) ? true : false);
      }
    }

    /// Classifies the provided configuration file line and returns a value indicating the
    /// result.
    /// @param [in] configLine Line read from the configuration file.
    /// @return Configuration line classification.
    static ELineClassification ClassifyConfigurationFileLine(std::wstring_view configLine)
    {
      // Skip over all whitespace at the start of the input line.
      while ((false == configLine.empty()) && (iswblank(configLine.front())))
        configLine.remove_prefix(1);

      // Sanity check: zero-length and all-whitespace lines can be safely ignored.
      // Also filter out comments this way.
      if (0 == configLine.length() || L';' == configLine.front() || L'#' == configLine.front())
        return ELineClassification::Ignore;

      // Non-comments must, by definition, have at least three characters in them, excluding
      // all whitespace. For section headers, this must mean '[' + section name + ']'. For
      // values, this must mean name + '=' + value.
      if (configLine.length() < 3) return ELineClassification::Error;

      if (L'[' == configLine[0])
      {
        // The line cannot be a section header unless the second character is a valid
        // section name character.
        if (!IsAllowedSectionCharacter(configLine[1])) return ELineClassification::Error;

        // Verify that the line is a valid section header by checking for valid section name
        // characters between two square brackets.
        size_t i = 2;
        for (; i < configLine.length() && L']' != configLine[i]; ++i)
          if (!IsAllowedSectionCharacter(configLine[i])) return ELineClassification::Error;
        if (L']' != configLine[i]) return ELineClassification::Error;

        // Verify that the remainder of the line is just whitespace.
        for (i += 1; i < configLine.length(); ++i)
          if (!iswblank(configLine[i])) return ELineClassification::Error;

        return ELineClassification::Section;
      }
      else if (IsAllowedNameCharacter(configLine[0]))
      {
        // Search for whitespace or an equals sign, with all characters in between needing
        // to be allowed as value name characters.
        size_t i = 1;
        for (; i < configLine.length() && L'=' != configLine[i] && !iswblank(configLine[i]); ++i)
          if (!IsAllowedNameCharacter(configLine[i])) return ELineClassification::Error;

        // Skip over any whitespace present, then check for an equals sign.
        for (; i < configLine.length() && iswblank(configLine[i]); ++i)
          ;
        if (L'=' != configLine[i]) return ELineClassification::Error;

        // Skip over any whitespace present, then verify the next character is allowed to
        // start a value setting.
        for (i += 1; i < configLine.length() && iswblank(configLine[i]); ++i)
          ;
        if (!IsAllowedValueCharacter(configLine[i])) return ELineClassification::Error;

        // Skip over the value setting characters that follow.
        for (i += 1; i < configLine.length() && IsAllowedValueCharacter(configLine[i]); ++i)
          ;

        // Verify that the remainder of the line is just whitespace.
        for (; i < configLine.length(); ++i)
        {
          if (!iswblank(configLine[i])) return ELineClassification::Error;
        }

        return ELineClassification::Value;
      }

      return ELineClassification::Error;
    }

    /// Parses a Boolean from the supplied input string.
    /// @param [in] source String from which to parse.
    /// @param [out] dest Filled with the result of the parse.
    /// @return `true` if the parse was successful and able to consume the whole string, `false`
    /// otherwise.
    static bool ParseBoolean(const std::wstring_view source, TBooleanValue& dest)
    {
      static constexpr std::wstring_view kTrueStrings[] = {
          L"t", L"true", L"on", L"y", L"yes", L"enabled", L"1"};
      static constexpr std::wstring_view kFalseStrings[] = {
          L"f", L"false", L"off", L"n", L"no", L"disabled", L"0"};

      // Check if the string represents a value of TRUE.
      for (auto& trueString : kTrueStrings)
      {
        if (true == Strings::EqualsCaseInsensitive(trueString, source))
        {
          dest = (TBooleanValue) true;
          return true;
        }
      }

      // Check if the string represents a value of FALSE.
      for (auto& falseString : kFalseStrings)
      {
        if (true == Strings::EqualsCaseInsensitive(falseString, source))
        {
          dest = (TBooleanValue) false;
          return true;
        }
      }

      return false;
    }

    /// Parses a signed integer value from the supplied input string.
    /// @param [in] source String from which to parse.
    /// @param [out] dest Filled with the result of the parse.
    /// @return `true` if the parse was successful and able to consume the whole string, `false`
    /// otherwise.
    static bool ParseInteger(std::wstring_view source, TIntegerValue& dest)
    {
      intmax_t value = 0;
      wchar_t* endptr = nullptr;

      // Parse out a number in any representable base.
      value = wcstoll(source.data(), &endptr, 0);

      // Verify that the number is not out of range.
      if (ERANGE == errno && (LLONG_MIN == value || LLONG_MAX == value)) return false;
      if ((value > std::numeric_limits<TIntegerValue>::max()) ||
          (value < std::numeric_limits<TIntegerValue>::min()))
        return false;

      // Verify that the whole string was consumed. This uses pointer arithmetic, and so the logic
      // holds regardless of the size of the individual characters.
      if (source.length() != (endptr - source.data())) return false;

      dest = (TIntegerValue)value;
      return true;
    }

    /// Parses a name and a value for the specified configuration file line, which must first
    /// have been classified as containing a name and value pair.
    /// @param [in] configLine Line read from the configuration file.
    /// @param [out] nameString Filled with the name of the configuration setting.
    /// @param [out] valueString Filled with the value specified for the configuration setting.
    static void ParseNameAndValue(
        std::wstring_view configLine, std::wstring_view& nameString, std::wstring_view& valueString)
    {
      const size_t equalSignPosition = configLine.find_first_of(L'=');
      nameString = Strings::TrimWhitespace(configLine.substr(0, equalSignPosition));
      valueString = Strings::TrimWhitespace(configLine.substr(1 + equalSignPosition));
    }

    /// Parses a section name from the specified configuration file line, which must first have
    /// been classified as containing a section name.
    /// @param [in] configLine Line read from the configuration file.
    /// @param String containing the name of the configuration section.
    static std::wstring_view ParseSection(std::wstring_view configLine)
    {
      std::wstring_view parsedSection = configLine;
      parsedSection.remove_prefix(1 + configLine.find_first_of(L'['));
      parsedSection.remove_suffix(configLine.length() - configLine.find_first_of(L']'));
      return parsedSection;
    }

    /// Reads a single line from the specified handle and verifies that it fits within the
    /// specified buffer. Default implementation does nothing and always returns an error.
    /// @tparam ReadHandleType Handle that implements the functions required to read one line at
    /// a time from an input source.
    /// @param [in] readHandle Handle to the configuration file data from which to read.
    /// @param [out] configLine Filled with text read from the specified file.
    /// @return `true` if the line was read successfully and fits within the provided buffer,
    /// `false` otherwise.
    template <typename ReadHandleType> static bool ReadLine(
        ReadHandleType& readHandle, TemporaryString& configLine)
    {
      return -1;
    }

    /// Reads a single line from the specified file handle and verifies that it fits within the
    /// specified buffer. This is a template specialization for reading from files.
    /// @param [in] fileHandle File handle for the configuration file.
    /// @param [out] configLine Filled with text read from the specified file.
    /// @return `true` if the line was read successfully and fits within the provided buffer,
    /// `false` otherwise.
    template <> static bool ReadLine<FileHandle>(
        FileHandle& fileHandle, TemporaryString& configLine)
    {
      // Results in a null-terminated string guaranteed, but might not be the whole line if
      // the buffer is too small.
      if (configLine.Data() != fgetws(configLine.Data(), configLine.Capacity(), fileHandle))
        return false;
      configLine.UnsafeSetSize(
          static_cast<unsigned int>(wcsnlen(configLine.Data(), configLine.Capacity())));

      return true;
    }

    /// Reads a single line from the specified in-memory configuration file data and verifies that
    /// it fits within the specified buffer. This is a template specialization for reading from an
    /// in-memory buffer.
    /// @param [in] memoryBufferHandle Handle object for reading from the in-memory buffer.
    /// @param [out] configLine Filled with text read from the specified file.
    /// @return `true` if the line was read successfully and fits within the provided buffer,
    /// `false` otherwise.
    template <> static bool ReadLine<MemoryBufferHandle>(
        MemoryBufferHandle& memoryBufferHandle, TemporaryString& configLine)
    {
      if (memoryBufferHandle.IsEndOfInput()) return false;

      size_t nextLineLength = memoryBufferHandle.remainingBuffer.find_first_of(L'\n');

      if (std::wstring_view::npos == nextLineLength)
        nextLineLength = memoryBufferHandle.remainingBuffer.length();
      else
        nextLineLength += 1;

      int numCharsWritten = 0;
      for (; numCharsWritten <
           std::min(nextLineLength, static_cast<size_t>((configLine.Capacity() - 1)));
           ++numCharsWritten)
        configLine[numCharsWritten] = memoryBufferHandle.remainingBuffer[numCharsWritten];

      memoryBufferHandle.remainingBuffer.remove_prefix(numCharsWritten);
      configLine.UnsafeSetSize(numCharsWritten);

      return true;
    }

    Value::Value(const Value& other)
        : configFileName(other.configFileName), lineNumber(other.lineNumber), type(other.type)
    {
      switch (other.type)
      {
        case EValueType::Integer:
        case EValueType::IntegerMultiValue:
          new (&intValue) TIntegerValue(other.intValue);
          break;

        case EValueType::Boolean:
        case EValueType::BooleanMultiValue:
          new (&boolValue) TBooleanValue(other.boolValue);
          break;

        case EValueType::String:
        case EValueType::StringMultiValue:
          new (&stringValue) TStringValue(other.stringValue);
          break;

        default:
          break;
      }
    }

    Value::Value(Value&& other) noexcept
        : configFileName(std::move(other.configFileName)),
          lineNumber(std::move(other.lineNumber)),
          type(std::move(other.type))
    {
      switch (other.type)
      {
        case EValueType::Integer:
        case EValueType::IntegerMultiValue:
          new (&intValue) TIntegerValue(std::move(other.intValue));
          break;

        case EValueType::Boolean:
        case EValueType::BooleanMultiValue:
          new (&boolValue) TBooleanValue(std::move(other.boolValue));
          break;

        case EValueType::String:
        case EValueType::StringMultiValue:
          new (&stringValue) TStringValue(std::move(other.stringValue));
          break;

        default:
          break;
      }
    }

    Value::~Value(void)
    {
      switch (type)
      {
        case EValueType::Integer:
        case EValueType::IntegerMultiValue:
          intValue.~TIntegerValue();
          break;

        case EValueType::Boolean:
        case EValueType::BooleanMultiValue:
          boolValue.~TBooleanValue();
          break;

        case EValueType::String:
        case EValueType::StringMultiValue:
          stringValue.~TStringValue();
          break;

        default:
          break;
      }
    }

    bool Value::operator<(const Value& rhs) const
    {
      const EValueType thisCanonicalType = CanonicalSingleValueType(GetType());
      const EValueType rhsCanonicalType = CanonicalSingleValueType(rhs.GetType());

      if (thisCanonicalType < rhsCanonicalType)
        return true;
      else if (thisCanonicalType > rhsCanonicalType)
        return false;

      switch (thisCanonicalType)
      {
        case EValueType::Integer:
          return (intValue < rhs.intValue);
        case EValueType::Boolean:
          return (boolValue < rhs.boolValue);
        case EValueType::String:
          return (stringValue < rhs.stringValue);
        default:
          return false;
      }
    }

    bool Value::operator==(const Value& rhs) const
    {
      switch (GetType())
      {
        case EValueType::Integer:
        case EValueType::IntegerMultiValue:
          return ((rhs.TypeIsInteger()) && (intValue == rhs.intValue));

        case EValueType::Boolean:
        case EValueType::BooleanMultiValue:
          return ((rhs.TypeIsBoolean()) && (boolValue == rhs.boolValue));

        case EValueType::String:
        case EValueType::StringMultiValue:
          return ((rhs.TypeIsString()) && (stringValue == rhs.stringValue));

        default:
          return false;
      }
    }

    template <> bool Value::TypeIs<TBooleanValue>(void) const
    {
      switch (GetType())
      {
        case EValueType::Boolean:
        case EValueType::BooleanMultiValue:
          return true;

        default:
          return false;
      }
    }

    template <> bool Value::TypeIs<TIntegerValue>(void) const
    {
      switch (GetType())
      {
        case EValueType::Integer:
        case EValueType::IntegerMultiValue:
          return true;

        default:
          return false;
      }
    }

    template <> bool Value::TypeIs<TStringValue>(void) const
    {
      switch (GetType())
      {
        case EValueType::String:
        case EValueType::StringMultiValue:
          return true;

        default:
          return false;
      }
    }

    bool Value::TypeIsInteger(void) const
    {
      return TypeIs<TIntegerValue>();
    }

    bool Value::TypeIsBoolean(void) const
    {
      return TypeIs<TBooleanValue>();
    }

    bool Value::TypeIsString(void) const
    {
      return TypeIs<TStringValue>();
    }

    template <> TBooleanValue Value::Extract(void)
    {
      DebugAssert(true == TypeIsBoolean(), "Object does not hold a Boolean value.");
      type = EValueType::Error;
      return std::move(boolValue);
    }

    template <> TIntegerValue Value::Extract(void)
    {
      DebugAssert(true == TypeIsInteger(), "Object does not hold an integer value.");
      type = EValueType::Error;
      return std::move(intValue);
    }

    template <> TStringValue Value::Extract(void)
    {
      DebugAssert(true == TypeIsString(), "Object does not hold a string value.");
      type = EValueType::Error;
      return std::move(stringValue);
    }

    TIntegerValue Value::ExtractInteger(void)
    {
      return Extract<TIntegerValue>();
    }

    TBooleanValue Value::ExtractBoolean(void)
    {
      return Extract<TBooleanValue>();
    }

    TStringValue Value::ExtractString(void)
    {
      return Extract<TStringValue>();
    }

    template <> TBooleanView Value::Get(void) const
    {
      return *this;
    }

    template <> TIntegerView Value::Get(void) const
    {
      return *this;
    }

    template <> TStringView Value::Get(void) const
    {
      return *this;
    }

    TIntegerView Value::GetInteger(void) const
    {
      return Get<TIntegerView>();
    }

    TBooleanView Value::GetBoolean(void) const
    {
      return Get<TBooleanView>();
    }

    TStringView Value::GetString(void) const
    {
      return Get<TStringView>();
    }

    template <typename ValueType> std::optional<ValueType> Name::ExtractFirst(void)
    {
      if (false == TypeIs<ValueType>()) return std::nullopt;
      return values.extract(values.begin()).value().Extract<ValueType>();
    }

    template std::optional<TBooleanValue> Name::ExtractFirst(void);
    template std::optional<TIntegerValue> Name::ExtractFirst(void);
    template std::optional<TStringValue> Name::ExtractFirst(void);

    template <typename ValueType> std::optional<std::vector<ValueType>> Name::ExtractAll(void)
    {
      if (false == TypeIs<ValueType>()) return std::nullopt;

      std::vector<ValueType> extractedValues;
      extractedValues.reserve(values.size());

      while (false == values.empty())
      {
        ValueType extractedValue = values.extract(values.begin()).value().Extract<ValueType>();
        extractedValues.emplace_back(std::move(extractedValue));
      }

      return std::move(extractedValues);
    }

    template std::optional<std::vector<TBooleanValue>> Name::ExtractAll<TBooleanValue>(void);
    template std::optional<std::vector<TIntegerValue>> Name::ExtractAll<TIntegerValue>(void);
    template std::optional<std::vector<TStringValue>> Name::ExtractAll<TStringValue>(void);

    std::optional<Name> Section::Extract(std::wstring_view name)
    {
      auto nameIterator = names.find(name);
      if (names.end() == nameIterator) return std::nullopt;

      auto extractedName = names.extract(nameIterator);
      return std::move(extractedName.mapped());
    }

    std::optional<std::pair<std::wstring, Name>> Section::ExtractFirst(void)
    {
      auto nameIterator = names.begin();
      if (names.end() == nameIterator) return std::nullopt;

      auto extractedName = names.extract(nameIterator);
      return std::make_pair(std::move(extractedName.key()), std::move(extractedName.mapped()));
    }

    std::pair<std::wstring, Section> ConfigurationData::ExtractSection(
        TSections::const_iterator position)
    {
      auto extractedSection = sections.extract(position);
      return std::make_pair(
          std::move(extractedSection.key()), std::move(extractedSection.mapped()));
    }

    void ConfigurationFileReader::AppendErrorMessage(std::wstring_view errorMessage)
    {
      if (false == errorMessages.has_value()) errorMessages.emplace();
      errorMessages->emplace_back(errorMessage);
    }

    ConfigurationData ConfigurationFileReader::ReadConfigurationFile(
        std::wstring_view configFileName, bool mustExist)
    {
      errorMessages.reset();

      FileHandle configFileHandle(configFileName.data(), L"r");
      if (nullptr == configFileHandle)
      {
        ConfigurationData configDataFromNonExistentFile;

        if (true == mustExist)
          AppendErrorMessage(Strings::Format(L"%s: Unable to open file.", configFileName.data()));

        return configDataFromNonExistentFile;
      }

      return ReadConfiguration(configFileHandle, configFileName);
    }

    ConfigurationData ConfigurationFileReader::ReadInMemoryConfigurationFile(
        std::wstring_view configBuffer)
    {
      errorMessages.reset();

      MemoryBufferHandle configBufferHandle(configBuffer);
      return ReadConfiguration(
          configBufferHandle,
          Strings::Format(
              L"[0x%0.*zx]",
              static_cast<int>(2 * sizeof(size_t)),
              reinterpret_cast<size_t>(configBuffer.data())));
    }

    template <typename ReadHandleType> ConfigurationData ConfigurationFileReader::ReadConfiguration(
        ReadHandleType& readHandle, std::wstring_view configSourceName)
    {
      ConfigurationData configToFill;

      BeginRead();

      // Parse the configuration file, one line at a time.
      std::set<std::wstring, Strings::CaseInsensitiveLessThanComparator<wchar_t>> seenSections;
      std::wstring_view thisSection = kSectionNameGlobal;

      int configLineNumber = 1;
      TemporaryString configLine;
      bool configLineReadResult = ReadLine(readHandle, configLine);
      bool skipValueLines = false;

      while (true == configLineReadResult)
      {
        configLine.TrimTrailingWhitespace();

        switch (ClassifyConfigurationFileLine(configLine))
        {
          case ELineClassification::Error:
            AppendErrorMessage(Strings::Format(
                L"%s(%d): Unable to parse line.", configSourceName.data(), configLineNumber));
            break;

          case ELineClassification::Ignore:
            break;

          case ELineClassification::Section:
          {
            std::wstring_view section = ParseSection(configLine);

            if (0 != seenSections.count(section))
            {
              AppendErrorMessage(Strings::Format(
                  L"%s(%d): %s: Duplicated section name.",
                  configSourceName.data(),
                  configLineNumber,
                  section.data()));
              skipValueLines = true;
              break;
            }

            const Action sectionAction = ActionForSection(section);
            switch (sectionAction.GetAction())
            {
              case EAction::Error:
                if (true == sectionAction.HasErrorMessage())
                  AppendErrorMessage(Strings::Format(
                      L"%s(%d): %s",
                      configSourceName.data(),
                      configLineNumber,
                      sectionAction.GetErrorMessage().c_str()));
                else
                  AppendErrorMessage(Strings::Format(
                      L"%s(%d): %s: Unrecognized section name.",
                      configSourceName.data(),
                      configLineNumber,
                      section.data()));
                skipValueLines = true;
                break;

              case EAction::Process:
                thisSection = *(seenSections.emplace(section).first);
                skipValueLines = false;
                break;

              case EAction::Skip:
                skipValueLines = true;
                break;

              default:
                AppendErrorMessage(Strings::Format(
                    L"%s(%d): Internal error while processing section name.",
                    configSourceName.data(),
                    configLineNumber));
                skipValueLines = true;
                break;
            }
            break;
          }

          case ELineClassification::Value:
            if (false == skipValueLines)
            {
              std::wstring_view name;
              std::wstring_view value;
              ParseNameAndValue(configLine, name, value);

              const auto& existingName = configToFill[thisSection][name];
              const EValueType valueType =
                  (existingName.HasValue() ? existingName.GetType()
                                           : TypeForValue(thisSection, name));
              bool shouldParseValue = true;

              // If the value type does not identify it as multi-valued, make sure
              // this is the first time the setting is seen.
              switch (valueType)
              {
                case EValueType::Integer:
                case EValueType::Boolean:
                case EValueType::String:
                  if (configToFill.Contains(thisSection, name))
                  {
                    AppendErrorMessage(Strings::Format(
                        L"%s(%d): %s: Only a single value is allowed for this setting.",
                        configSourceName.data(),
                        configLineNumber,
                        name.data()));
                    shouldParseValue = false;
                  }
                  break;

                default:
                  break;
              }

              if (false == shouldParseValue) break;

              // Attempt to parse the value.
              switch (valueType)
              {
                case EValueType::Error:
                  AppendErrorMessage(Strings::Format(
                      L"%s(%d): %s: Unrecognized configuration setting.",
                      configSourceName.data(),
                      configLineNumber,
                      name.data()));
                  break;

                case EValueType::Integer:
                case EValueType::IntegerMultiValue:
                {
                  TIntegerValue intValue = (TIntegerValue)0;

                  if (false == ParseInteger(value, intValue))
                  {
                    AppendErrorMessage(Strings::Format(
                        L"%s(%d): %s: Failed to parse integer value.",
                        configSourceName.data(),
                        configLineNumber,
                        value.data()));
                    break;
                  }

                  const Action valueAction = ActionForValue(thisSection, name, intValue);
                  switch (valueAction.GetAction())
                  {
                    case EAction::Error:
                      if (true == valueAction.HasErrorMessage())
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s",
                            configSourceName.data(),
                            configLineNumber,
                            valueAction.GetErrorMessage().c_str()));
                      else
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s: Invalid value for configuration setting %s.",
                            configSourceName.data(),
                            configLineNumber,
                            value.data(),
                            name.data()));
                      break;

                    case EAction::Process:
                      if (false ==
                          configToFill.Insert(
                              thisSection,
                              name,
                              Value(
                                  TIntegerValue(intValue),
                                  valueType,
                                  configSourceName,
                                  configLineNumber)))
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s: Duplicated value for configuration setting %s.",
                            configSourceName.data(),
                            configLineNumber,
                            value.data(),
                            name.data()));
                      break;
                  }
                  break;
                }

                case EValueType::Boolean:
                case EValueType::BooleanMultiValue:
                {
                  TBooleanValue boolValue = (TBooleanValue) false;

                  if (false == ParseBoolean(value, boolValue))
                  {
                    AppendErrorMessage(Strings::Format(
                        L"%s(%d): %s: Failed to parse Boolean value.",
                        configSourceName.data(),
                        configLineNumber,
                        value.data()));
                    break;
                  }

                  const Action valueAction = ActionForValue(thisSection, name, boolValue);
                  switch (valueAction.GetAction())
                  {
                    case EAction::Error:
                      if (true == valueAction.HasErrorMessage())
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s",
                            configSourceName.data(),
                            configLineNumber,
                            valueAction.GetErrorMessage().c_str()));
                      else
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s: Invalid value for configuration setting %s.",
                            configSourceName.data(),
                            configLineNumber,
                            value.data(),
                            name.data()));
                      break;

                    case EAction::Process:
                      if (false ==
                          configToFill.Insert(
                              thisSection,
                              name,
                              Value(
                                  TBooleanValue(boolValue),
                                  valueType,
                                  configSourceName,
                                  configLineNumber)))
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s: Duplicated value for configuration setting %s.",
                            configSourceName.data(),
                            configLineNumber,
                            value.data(),
                            name.data()));
                      break;
                  }
                  break;
                }

                case EValueType::String:
                case EValueType::StringMultiValue:
                {
                  const Action valueAction = ActionForValue(thisSection, name, value);
                  switch (valueAction.GetAction())
                  {
                    case EAction::Error:
                      if (true == valueAction.HasErrorMessage())
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s",
                            configSourceName.data(),
                            configLineNumber,
                            valueAction.GetErrorMessage().c_str()));
                      else
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s: Invalid value for configuration setting %s.",
                            configSourceName.data(),
                            configLineNumber,
                            value.data(),
                            name.data()));
                      break;

                    case EAction::Process:
                      if (false ==
                          configToFill.Insert(
                              thisSection,
                              name,
                              Value(
                                  TStringValue(value),
                                  valueType,
                                  configSourceName,
                                  configLineNumber)))
                        AppendErrorMessage(Strings::Format(
                            L"%s(%d): %s: Duplicated value for configuration setting %s.",
                            configSourceName.data(),
                            configLineNumber,
                            value.data(),
                            name.data()));
                      break;
                  }
                  break;
                }

                default:
                  AppendErrorMessage(Strings::Format(
                      L"%s(%d): Internal error while processing configuration setting.",
                      configSourceName.data(),
                      configLineNumber));
                  break;
              }
            }
            break;

          default:
            AppendErrorMessage(Strings::Format(
                L"%s(%d): Internal error while processing line.",
                configSourceName.data(),
                configLineNumber));
            break;
        }

        configLine.Clear();
        configLineReadResult = ReadLine(readHandle, configLine);
        configLineNumber += 1;
      }

      if (!readHandle.IsEndOfInput())
      {
        // Stopped reading the configuration file early due to some condition other than
        // end-of-file. This indicates an error.

        if (readHandle.IsError())
        {
          AppendErrorMessage(Strings::Format(
              L"%s(%d): I/O error while reading.", configSourceName.data(), configLineNumber));
          return configToFill;
        }
        else if (false == configLineReadResult)
        {
          AppendErrorMessage(Strings::Format(
              L"%s(%d): Line is too long.", configSourceName.data(), configLineNumber));
          return configToFill;
        }
      }

      EndRead();

      return configToFill;
    }

    std::wstring ConfigurationData::ToConfigurationFileString(void) const
    {
      std::wstringstream configurationFileStringStream;

      for (const auto& sectionRecord : sections)
      {
        std::wstring_view section = sectionRecord.first;

        if (false == section.empty())
          configurationFileStringStream << L'[' << sectionRecord.first << L']' << L'\n';

        for (const auto& nameRecord : sectionRecord.second.Names())
        {
          std::wstring_view name = nameRecord.first;

          for (const auto& valueRecord : nameRecord.second.Values())
          {
            switch (valueRecord.GetType())
            {
              case EValueType::Integer:
              case EValueType::IntegerMultiValue:
                configurationFileStringStream << name << L" = " << valueRecord.GetInteger()
                                              << L'\n';
                break;

              case EValueType::Boolean:
              case EValueType::BooleanMultiValue:
                configurationFileStringStream
                    << name << L" = " << ((true == valueRecord.GetBoolean()) ? L"yes" : L"no")
                    << L'\n';
                break;

              case EValueType::String:
              case EValueType::StringMultiValue:
                configurationFileStringStream << name << L" = " << valueRecord.GetString() << L'\n';
                break;
            }
          }
        }
      }

      return configurationFileStringStream.str();
    }

    void ConfigurationFileReader::BeginRead(void) {}

    void ConfigurationFileReader::EndRead(void) {}
  } // namespace Configuration
} // namespace Infra
