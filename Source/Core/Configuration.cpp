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
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>
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

    /// Base class containing common functionality for reading from a configuration file. Abstracts
    /// away the details of reading from various sources, including files and memory buffers.
    class ConfigSourceReaderBase
    {
    public:

      constexpr ConfigSourceReaderBase(void) : lastReadConfigLineNumber(0) {}

      virtual ~ConfigSourceReaderBase(void) = default;

      /// Retrieves a string identifying the name of the configuration source.
      /// @return String identifier of the config source name.
      virtual std::wstring_view GetConfigSourceName(void) const = 0;

      /// Determines if the end of the input has been reached.
      /// @return `true` if no more input is available, `false` otherwise.
      virtual bool IsEndOfInput(void) const = 0;

      /// Determines if an error has occurred while reading from the input source.
      /// @return `true` if an error has occurred, `false` otherwise.
      virtual bool IsError(void) const = 0;

      /// Retrieves and returns the line number of the last read configuration line.
      /// @return Line number of the last call to #ReadLine, or 0 if no calls have been made.
      inline unsigned int GetLastReadConfigLineNumber(void) const
      {
        return lastReadConfigLineNumber;
      }

      /// Reads the next line from the input source and stores it into the specified temporary
      /// string object.
      /// @param [out] configLine Filled with the contents of the next line from the input source.
      /// @return `true` if the line was read successfully and fits within the provided buffer,
      /// `false` otherwise.
      inline bool ReadLine(TemporaryString& configLine)
      {
        const bool readLineResult = ReadLineImpl(configLine);
        if (true == readLineResult) lastReadConfigLineNumber += 1;
        return readLineResult;
      }

    protected:

      /// Internal implementation of reading the next line from the input source, specific to each
      /// type of input source.
      /// @param [out] configLine Filled with the contents of the next line from the input source.
      /// @return `true` if the line was read successfully and fits within the provided buffer,
      /// `false` otherwise.
      virtual bool ReadLineImpl(TemporaryString& configLine) = 0;

    private:

      /// Line number of the last call to #ReadLine.
      unsigned int lastReadConfigLineNumber;
    };

    /// Reader that uses a file as input. Attempts to open the specified file on construction and
    /// close it on destruction.
    class FileReader : public ConfigSourceReaderBase
    {
    public:

      inline FileReader(const wchar_t* fileName)
          : ConfigSourceReaderBase(), fileName(fileName), fileHandle(nullptr)
      {
        _wfopen_s(&fileHandle, fileName, L"r");
      }

      FileReader(const FileReader& other) = delete;

      ~FileReader(void) override
      {
        if (nullptr != fileHandle) fclose(fileHandle);
      }

      // ConfigSourceReaderBase
      std::wstring_view GetConfigSourceName(void) const override
      {
        return fileName;
      }

      bool IsEndOfInput(void) const override
      {
        return feof(fileHandle);
      }

      bool IsError(void) const override
      {
        return ((nullptr == fileHandle) || (ferror(fileHandle)));
      }

      bool ReadLineImpl(TemporaryString& configLine) override
      {
        // Results in a null-terminated string guaranteed, but might not be the whole line if
        // the buffer is too small.
        if (configLine.Data() != fgetws(configLine.Data(), configLine.Capacity(), fileHandle))
          return false;
        configLine.UnsafeSetSize(
            static_cast<unsigned int>(wcsnlen(configLine.Data(), configLine.Capacity())));
        return true;
      }

    private:

      /// Name of the file that is being read.
      std::wstring fileName;

      /// Handle used to read from the input source file.
      FILE* fileHandle;
    };

    /// Reader that uses an in-memory buffer as input. Primarily used for tests, and does not claim
    /// any ownership over the buffer itself.
    class MemoryBufferReader : public ConfigSourceReaderBase
    {
    public:

      inline MemoryBufferReader(std::wstring_view configBuffer)
          : ConfigSourceReaderBase(),
            configSourceName(Strings::Format(
                L"[0x%0.*zx]",
                static_cast<int>(2 * sizeof(size_t)),
                reinterpret_cast<size_t>(configBuffer.data()))),
            remainingBuffer(configBuffer)
      {}

      // ConfigSourceReaderBase
      std::wstring_view GetConfigSourceName(void) const override
      {
        return configSourceName;
      }

      bool IsEndOfInput(void) const override
      {
        return remainingBuffer.empty();
      }

      bool IsError(void) const override
      {
        return false;
      }

      bool ReadLineImpl(TemporaryString& configLine) override
      {
        if (IsEndOfInput()) return false;

        size_t nextLineLength = remainingBuffer.find_first_of(L'\n');
        if (std::wstring_view::npos == nextLineLength)
          nextLineLength = remainingBuffer.length();
        else
          nextLineLength += 1;

        if (nextLineLength > (static_cast<size_t>(configLine.Capacity()) - 1)) return false;
        configLine = std::wstring_view(remainingBuffer.data(), nextLineLength);
        remainingBuffer.remove_prefix(nextLineLength);
        return true;
      }

    private:

      /// String that identifies this memory buffer as a configuration source.
      std::wstring configSourceName;

      /// Remaining contents of the memory buffer.
      std::wstring_view remainingBuffer;
    };

    /// Determines and returns the canonical single-valued type for each possible multi-valued type
    /// enumerator.
    /// @param [in] valueType Type for which a canonical single-valued enumerator is desired.
    /// @return Canonical single-valued enumerator, which might be the same as the input parameter.
    static constexpr EValueType CanonicalSingleValueType(EValueType valueType)
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

    /// Determines if the specified type allows multiple values.
    /// @param [in] type Type enumerator to check.
    /// @return `true` if multiple values are allowed, `false` otherwise.
    static constexpr bool TypeAllowsMultipleValues(EValueType type)
    {
      switch (type)
      {
        case EValueType::IntegerMultiValue:
        case EValueType::BooleanMultiValue:
        case EValueType::StringMultiValue:
          return true;

        default:
          return false;
      }
    }

    /// Retrieves a string representation of the specified value type.
    /// @param [in] type Value type for which a string representation is desired.
    /// @return String representation of the value type.
    static constexpr std::wstring_view TypeToString(EValueType type)
    {
      switch (type)
      {
        case EValueType::Integer:
        case EValueType::IntegerMultiValue:
          return L"integer";

        case EValueType::Boolean:
        case EValueType::BooleanMultiValue:
          return L"Boolean";

        case EValueType::String:
        case EValueType::StringMultiValue:
          return L"string";

        case EValueType::Error:
          return L"(error type)";

        default:
          return L"(unknown type)";
      }
    }

    /// Converts the specified value type to its associated single-valued enumerator.
    /// @tparam ValueType Value type for which an associated enumerator is requested.
    /// @return Associated enumerator.
    template <typename ValueType> static consteval EValueType ValueTypeToSingleValueEnumerator(void)
    {
      if constexpr (std::is_same_v<ValueType, TIntegerValue>)
        return EValueType::Integer;
      else if constexpr (std::is_same_v<ValueType, TBooleanValue>)
        return EValueType::Boolean;
      else if constexpr (std::is_same_v<ValueType, TStringValue>)
        return EValueType::String;
      else
        return EValueType::Error;
    }

    /// Converts the specified value type to its associated multi-valued enumerator.
    /// @tparam ValueType Value type for which an associated enumerator is requested.
    /// @return Associated enumerator.
    template <typename ValueType> static consteval EValueType ValueTypeToMultiValueEnumerator(void)
    {
      if constexpr (std::is_same_v<ValueType, TIntegerValue>)
        return EValueType::IntegerMultiValue;
      else if constexpr (std::is_same_v<ValueType, TBooleanValue>)
        return EValueType::BooleanMultiValue;
      else if constexpr (std::is_same_v<ValueType, TStringValue>)
        return EValueType::StringMultiValue;
      else
        return EValueType::Error;
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
          return (std::iswalnum(charToTest) ? true : false);
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
          return (std::iswalnum(charToTest) ? true : false);
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
          return (std::iswalnum(charToTest) ? true : false);
      }
    }

    /// Classifies the provided configuration file line and returns a value indicating the
    /// result.
    /// @param [in] configLine Line read from the configuration file, with whitespace already
    /// trimmed.
    /// @return Configuration line classification.
    static ELineClassification ClassifyConfigurationFileLine(std::wstring_view configLineTrimmed)
    {
      // Sanity check: zero-length and all-whitespace lines can be safely ignored.
      // Also filter out comments this way.
      if (0 == configLineTrimmed.length() || L';' == configLineTrimmed.front() ||
          L'#' == configLineTrimmed.front())
        return ELineClassification::Ignore;

      // Non-comments must, by definition, have at least three characters in them, excluding
      // all whitespace. For section headers, this must mean '[' + section name + ']'. For
      // values, this must mean name + '=' + value.
      if (configLineTrimmed.length() < 3) return ELineClassification::Error;

      if (L'[' == configLineTrimmed[0])
      {
        // The line cannot be a section header unless the second character is a valid
        // section name character.
        if (!IsAllowedSectionCharacter(configLineTrimmed[1])) return ELineClassification::Error;

        // Verify that the line is a valid section header by checking for valid section name
        // characters between two square brackets.
        size_t i = 2;
        for (; i < configLineTrimmed.length() && L']' != configLineTrimmed[i]; ++i)
          if (!IsAllowedSectionCharacter(configLineTrimmed[i])) return ELineClassification::Error;
        if (L']' != configLineTrimmed[i]) return ELineClassification::Error;

        // Verify that the remainder of the line is just whitespace.
        for (i += 1; i < configLineTrimmed.length(); ++i)
          if (!std::iswblank(configLineTrimmed[i])) return ELineClassification::Error;

        return ELineClassification::Section;
      }
      else if (IsAllowedNameCharacter(configLineTrimmed[0]))
      {
        // Search for whitespace or an equals sign, with all characters in between needing
        // to be allowed as value name characters.
        size_t i = 1;
        for (; i < configLineTrimmed.length() && L'=' != configLineTrimmed[i] &&
             !std::iswblank(configLineTrimmed[i]);
             ++i)
          if (!IsAllowedNameCharacter(configLineTrimmed[i])) return ELineClassification::Error;

        // Skip over any whitespace present, then check for an equals sign.
        for (; i < configLineTrimmed.length() && std::iswblank(configLineTrimmed[i]); ++i)
          ;
        if (L'=' != configLineTrimmed[i]) return ELineClassification::Error;

        // Skip over any whitespace present, then verify the next character is allowed to
        // start a value setting.
        for (i += 1; i < configLineTrimmed.length() && std::iswblank(configLineTrimmed[i]); ++i)
          ;
        if (!IsAllowedValueCharacter(configLineTrimmed[i])) return ELineClassification::Error;

        // Skip over the value setting characters that follow.
        for (i += 1;
             i < configLineTrimmed.length() && IsAllowedValueCharacter(configLineTrimmed[i]);
             ++i)
          ;

        // Verify that the remainder of the line is just whitespace.
        for (; i < configLineTrimmed.length(); ++i)
        {
          if (!std::iswblank(configLineTrimmed[i])) return ELineClassification::Error;
        }

        return ELineClassification::Value;
      }

      return ELineClassification::Error;
    }

    /// Parses a value from the supplied input string. Default implementation does nothing and
    /// always fails.
    /// @param [in] source String from which to parse.
    /// @param [out] dest Filled with the result of the parse.
    /// @return `false` because this is the default implementation that does nothing.
    template <typename ValueType> bool ParseTypedValue(std::wstring_view source, ValueType& dest)
    {
      return false;
    }

    /// Parses a signed integer value from the supplied input string.
    /// @param [in] source String from which to parse.
    /// @param [out] dest Filled with the result of the parse.
    /// @return `true` if the parse was successful and able to consume the whole string, `false`
    /// otherwise.
    template <> bool ParseTypedValue(std::wstring_view source, TIntegerValue& dest)
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

      dest = static_cast<TIntegerValue>(value);
      return true;
    }

    /// Parses a Boolean value from the supplied input string.
    /// @param [in] source String from which to parse.
    /// @param [out] dest Filled with the result of the parse.
    /// @return `true` if the parse was successful and able to consume the whole string, `false`
    /// otherwise.
    template <> bool ParseTypedValue(std::wstring_view source, TBooleanValue& dest)
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

    /// Parses a string value from the supplied input string. This is a no-op.
    /// @param [in] source String from which to parse.
    /// @param [out] dest Filled with the result of the parse.
    /// @return `true` because string parsing to a string is a no-op.
    template <> bool ParseTypedValue(std::wstring_view source, TStringValue& dest)
    {
      dest = source;
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
      DebugAssert(
          std::wstring_view::npos != configLine.find_first_of(L'='),
          "Attempting to parse a name/value pair from a line that does not contain one.");

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
      DebugAssert(
          ((std::wstring_view::npos != configLine.find_first_of(L'[')) &&
           (std::wstring_view::npos != configLine.find_first_of(L']')) &&
           (configLine.find_first_of(L']') > configLine.find_first_of(L'['))),
          "Attempting to parse a section name from a line that does not contain one.");

      std::wstring_view parsedSection = configLine;
      parsedSection.remove_prefix(1 + configLine.find_first_of(L'['));
      parsedSection.remove_suffix(configLine.length() - configLine.find_first_of(L']'));
      return parsedSection;
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

      std::unique_ptr<ConfigSourceReaderBase> fileReader =
          std::make_unique<FileReader>(configFileName.data());
      if (true == fileReader->IsError())
      {
        if (true == mustExist)
          AppendErrorMessage(Strings::Format(
              L"%.*s: Unable to open file.",
              static_cast<int>(configFileName.length()),
              configFileName.data()));
        return ConfigurationData();
      }

      return ReadConfiguration(std::move(fileReader));
    }

    ConfigurationData ConfigurationFileReader::ReadInMemoryConfigurationFile(
        std::wstring_view configBuffer)
    {
      errorMessages.reset();
      std::unique_ptr<ConfigSourceReaderBase> memoryBufferReader =
          std::make_unique<MemoryBufferReader>(configBuffer);
      return ReadConfiguration(std::move(memoryBufferReader));
    }

    void ConfigurationFileReader::ParseAndMaybeInsertSection(
        const ConfigSourceReaderBase* reader,
        ConfigurationData& configToFill,
        std::wstring_view configLineTrimmed,
        std::set<std::wstring, Strings::CaseInsensitiveLessThanComparator<wchar_t>>& seenSections,
        std::wstring_view& currentSection,
        bool& skipValueLines)
    {
      std::wstring_view section = ParseSection(configLineTrimmed);
      if (0 != seenSections.count(section))
      {
        AppendErrorMessage(Strings::Format(
            L"%.*s(%u): %.*s: Duplicated section name.",
            static_cast<int>(reader->GetConfigSourceName().length()),
            reader->GetConfigSourceName().data(),
            reader->GetLastReadConfigLineNumber(),
            static_cast<int>(section.length()),
            section.data()));
        skipValueLines = true;
        return;
      }

      const Action sectionAction = ActionForSection(section);
      switch (sectionAction.GetAction())
      {
        case EAction::Error:
          if (true == sectionAction.HasErrorMessage())
            AppendErrorMessage(Strings::Format(
                L"%.*s(%u): %s",
                static_cast<int>(reader->GetConfigSourceName().length()),
                reader->GetConfigSourceName().data(),
                reader->GetLastReadConfigLineNumber(),
                sectionAction.GetErrorMessage().c_str()));
          else
            AppendErrorMessage(Strings::Format(
                L"%.*s(%u): %.*s: Unrecognized section name.",
                static_cast<int>(reader->GetConfigSourceName().length()),
                reader->GetConfigSourceName().data(),
                reader->GetLastReadConfigLineNumber(),
                static_cast<int>(section.length()),
                section.data()));
          skipValueLines = true;
          break;

        case EAction::Process:
          currentSection = *(seenSections.emplace(section).first);
          skipValueLines = false;
          break;

        case EAction::Skip:
          skipValueLines = true;
          break;

        default:
          AppendErrorMessage(Strings::Format(
              L"%.*s(%u): Internal error while processing section name.",
              static_cast<int>(reader->GetConfigSourceName().length()),
              reader->GetConfigSourceName().data(),
              reader->GetLastReadConfigLineNumber()));
          skipValueLines = true;
          break;
      }
    }

    template <typename ValueType> void ConfigurationFileReader::ParseAndMaybeInsertValue(
        const ConfigSourceReaderBase* reader,
        ConfigurationData& configToFill,
        std::wstring_view section,
        std::wstring_view name,
        EValueType valueType,
        std::wstring_view valueUnparsed)
    {
      ValueType valueParsed{};
      if (false == ParseTypedValue(valueUnparsed, valueParsed))
      {
        std::wstring_view typeString = TypeToString(ValueTypeToSingleValueEnumerator<ValueType>());
        AppendErrorMessage(Strings::Format(
            L"%.*s(%u): %.*s: Failed to parse %.*s value.",
            static_cast<int>(reader->GetConfigSourceName().length()),
            reader->GetConfigSourceName().data(),
            reader->GetLastReadConfigLineNumber(),
            static_cast<int>(valueUnparsed.length()),
            valueUnparsed.data(),
            static_cast<int>(typeString.length()),
            typeString.data()));
        return;
      }

      const Action valueAction = ActionForValue(section, name, valueParsed);
      switch (valueAction.GetAction())
      {
        case EAction::Error:
          if (true == valueAction.HasErrorMessage())
            AppendErrorMessage(Strings::Format(
                L"%.*s(%u): %s",
                static_cast<int>(reader->GetConfigSourceName().length()),
                reader->GetConfigSourceName().data(),
                reader->GetLastReadConfigLineNumber(),
                valueAction.GetErrorMessage().c_str()));
          else
            AppendErrorMessage(Strings::Format(
                L"%.*s(%u): %.*s: Invalid value for configuration setting %.*s.",
                static_cast<int>(reader->GetConfigSourceName().length()),
                reader->GetConfigSourceName().data(),
                reader->GetLastReadConfigLineNumber(),
                static_cast<int>(valueUnparsed.length()),
                valueUnparsed.data(),
                static_cast<int>(name.length()),
                name.data()));
          break;

        case EAction::Process:
          if (false ==
              configToFill.Insert(
                  section,
                  name,
                  Value(
                      std::move(valueParsed),
                      valueType,
                      reader->GetConfigSourceName(),
                      reader->GetLastReadConfigLineNumber())))
            AppendErrorMessage(Strings::Format(
                L"%.*s(%u): %.*s: Duplicated value for configuration setting %.*s.",
                static_cast<int>(reader->GetConfigSourceName().length()),
                reader->GetConfigSourceName().data(),
                reader->GetLastReadConfigLineNumber(),
                static_cast<int>(valueUnparsed.length()),
                valueUnparsed.data(),
                static_cast<int>(name.length()),
                name.data()));
          break;
      }
    }

    ConfigurationData ConfigurationFileReader::ReadConfiguration(
        std::unique_ptr<ConfigSourceReaderBase>&& reader)
    {
      ConfigurationData configToFill;

      BeginRead();

      // Parse the configuration file, one line at a time.
      std::set<std::wstring, Strings::CaseInsensitiveLessThanComparator<wchar_t>> seenSections;
      std::wstring_view currentSection = kSectionNameGlobal;

      TemporaryString configLine;
      bool configLineReadResult = reader->ReadLine(configLine);
      bool skipValueLines = false;

      while (true == configLineReadResult)
      {
        std::wstring_view configLineTrimmed = Strings::TrimWhitespace(configLine.AsStringView());

        switch (ClassifyConfigurationFileLine(configLineTrimmed))
        {
          case ELineClassification::Error:
            AppendErrorMessage(Strings::Format(
                L"%.*s(%u): Unable to parse line.",
                static_cast<int>(reader->GetConfigSourceName().length()),
                reader->GetConfigSourceName().data(),
                reader->GetLastReadConfigLineNumber()));
            break;

          case ELineClassification::Ignore:
            break;

          case ELineClassification::Section:
            ParseAndMaybeInsertSection(
                reader.get(),
                configToFill,
                configLineTrimmed,
                seenSections,
                currentSection,
                skipValueLines);
            break;

          case ELineClassification::Value:
            if (false == skipValueLines)
            {
              std::wstring_view name;
              std::wstring_view value;
              ParseNameAndValue(configLineTrimmed, name, value);

              const auto& existingName = configToFill[currentSection][name];
              const EValueType valueType =
                  (existingName.HasValue() ? existingName.GetType()
                                           : TypeForValue(currentSection, name));

              // If the value type does not identify it as multi-valued, make sure
              // this is the first time the setting is seen.
              if ((false == TypeAllowsMultipleValues(valueType)) &&
                  (configToFill.Contains(currentSection, name)))
              {
                AppendErrorMessage(Strings::Format(
                    L"%.*s(%u): %.*s: Only a single value is allowed for this setting.",
                    static_cast<int>(reader->GetConfigSourceName().length()),
                    reader->GetConfigSourceName().data(),
                    reader->GetLastReadConfigLineNumber(),
                    static_cast<int>(name.length()),
                    name.data()));
                break;
              }

              switch (valueType)
              {
                case EValueType::Error:
                  AppendErrorMessage(Strings::Format(
                      L"%.*s(%u): %.*s: Unrecognized configuration setting.",
                      static_cast<int>(reader->GetConfigSourceName().length()),
                      reader->GetConfigSourceName().data(),
                      reader->GetLastReadConfigLineNumber(),
                      static_cast<int>(name.length()),
                      name.data()));
                  break;

                case EValueType::Integer:
                case EValueType::IntegerMultiValue:
                  ParseAndMaybeInsertValue<TIntegerValue>(
                      reader.get(), configToFill, currentSection, name, valueType, value);
                  break;

                case EValueType::Boolean:
                case EValueType::BooleanMultiValue:
                  ParseAndMaybeInsertValue<TBooleanValue>(
                      reader.get(), configToFill, currentSection, name, valueType, value);
                  break;

                case EValueType::String:
                case EValueType::StringMultiValue:
                  ParseAndMaybeInsertValue<TStringValue>(
                      reader.get(), configToFill, currentSection, name, valueType, value);
                  break;

                default:
                  AppendErrorMessage(Strings::Format(
                      L"%.*s(%u): Internal error while processing configuration setting.",
                      static_cast<int>(reader->GetConfigSourceName().length()),
                      reader->GetConfigSourceName().data(),
                      reader->GetLastReadConfigLineNumber()));
                  break;
              }
            }
            break;

          default:
            AppendErrorMessage(Strings::Format(
                L"%.*s(%u): Internal error while processing line.",
                static_cast<int>(reader->GetConfigSourceName().length()),
                reader->GetConfigSourceName().data(),
                reader->GetLastReadConfigLineNumber()));
            break;
        }

        configLineReadResult = reader->ReadLine(configLine);
      }

      if (false == reader->IsEndOfInput())
      {
        // Stopped reading the configuration file early due to some condition other than
        // end-of-file. This indicates an error.

        if (true == reader->IsError())
        {
          AppendErrorMessage(Strings::Format(
              L"%.*s(%u): I/O error while reading.",
              static_cast<int>(reader->GetConfigSourceName().length()),
              reader->GetConfigSourceName().data(),
              reader->GetLastReadConfigLineNumber()));
          return configToFill;
        }
        else if (false == configLineReadResult)
        {
          AppendErrorMessage(Strings::Format(
              L"%.*s(%u): Line is too long.",
              static_cast<int>(reader->GetConfigSourceName().length()),
              reader->GetConfigSourceName().data(),
              reader->GetLastReadConfigLineNumber()));
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
