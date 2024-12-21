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
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include "Core/DebugAssert.h"
#include "Core/Strings.h"
#include "Core/TemporaryBuffer.h"

namespace Infra
{
  namespace Configuration
  {
    /// Enumerates all possible classifications of configuration file lines. Used during parsing to
    /// classify each line encountered.
    enum class ELineClassification
    {
      /// Line could not be parsed.
      Error,

      /// Line contains an directive that provides an instruction on how to process the
      /// configuration file.
      Directive,

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
    class ConfigSourceReader
    {
    public:

      constexpr ConfigSourceReader(void) : lastReadConfigLineNumber(0) {}

      virtual ~ConfigSourceReader(void) = default;

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
    class FileReader : public ConfigSourceReader
    {
    public:

      inline FileReader(std::wstring_view fileName)
          : ConfigSourceReader(), fileName(fileName), fileHandle(nullptr)
      {
        _wfopen_s(&fileHandle, this->fileName.c_str(), L"r");
      }

      FileReader(const FileReader& other) = delete;

      ~FileReader(void) override
      {
        if (nullptr != fileHandle) fclose(fileHandle);
      }

      // ConfigSourceReader
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
    class MemoryBufferReader : public ConfigSourceReader
    {
    public:

      inline MemoryBufferReader(std::wstring_view configBuffer)
          : ConfigSourceReader(),
            configSourceName(Strings::Format(
                L"[0x%0.*zx]",
                static_cast<int>(2 * sizeof(size_t)),
                reinterpret_cast<size_t>(configBuffer.data()))),
            remainingBuffer(configBuffer)
      {}

      // ConfigSourceReader
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

    /// Structure for holding the state of an overall configuration file read operation.
    struct SReadState
    {
      /// Stack that holds all reader objects, most recent first. When an "include" directive is
      /// encountered in a configuration file, a new reader is pushed onto this stack.
      TemporaryVector<std::unique_ptr<ConfigSourceReader>> readers;

      /// Configuration data to be filled during the read operation.
      ConfigurationData configToFill;

      /// Set of all sections whose names have already been seen. Used to detect duplicate sections.
      std::set<std::wstring, Strings::CaseInsensitiveLessThanComparator<wchar_t>> seenSections;

      /// Name of the current section into which configuration settings should be inserted.
      std::wstring_view currentSection;

      struct
      {
        /// If `true` then all of the values in the current section should be skipped.
        bool skipValueLines : 1;

        /// If `true` then in-memory configuration files are allowed to be read.
        bool allowReadFromInMemoryConfigFiles : 1;
      };

      inline SReadState(void)
          : readers(),
            configToFill(),
            seenSections({std::wstring(kSectionNameGlobal)}),
            currentSection(kSectionNameGlobal),
            skipValueLines(false),
            allowReadFromInMemoryConfigFiles(false)
      {}

      /// Retrieves a mutable reference to the config source reader at the top of the stack.
      /// @return Config source reader at the top of the stack.
      inline ConfigSourceReader& GetCurrentReader(void)
      {
        DebugAssert(false == readers.Empty(), "No current reader defined.");
        return *readers.Back().get();
      }

      /// Retrieves a read-only reference to the config source reader at the top of the stack.
      /// @return Config source reader at the top of the stack.
      inline const ConfigSourceReader& GetCurrentReader(void) const
      {
        DebugAssert(false == readers.Empty(), "No current reader defined.");
        return *readers.Back().get();
      }
    };

    constexpr int x = sizeof(SReadState);

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

      // Non-comments and non-directives must, by definition, have at least three characters in
      // them, excluding all whitespace. For section headers, this must mean '[' + section name +
      // ']'. For values, this must mean name + '=' + value. For directives, this means '%' plus
      // at least two other characters.
      if (configLineTrimmed.length() < 3) return ELineClassification::Error;

      // Directives begin with a percent sign. The validity of a directive is a semantic decision
      // made later on a per-directive basis.
      if (L'%' == configLineTrimmed.front()) return ELineClassification::Directive;

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

    /// Attempts to parse an assignment string into a left-hand-side string (the target of the
    /// assignment) and a right-hand-side string (the value being assigned).
    /// @param [in] strUnparsed Unparsed string that could be containing the assignment statement.
    /// @param [out] lhs Parsed left-hand-side of the assignment statement.
    /// @param [out] rhs Parsed right-hand-side of the assignment statement.
    /// @param [in] separator Character that separates the left side of the assignment from the
    /// right side. Typically this is an equals sign, which is the default.
    /// @return `true` if the supplied statement was successfully parsed as an assignment statement,
    /// `false` otherwise.
    static bool ParseAssignmentString(
        std::wstring_view strUnparsed,
        std::wstring_view& lhs,
        std::wstring_view& rhs,
        wchar_t separator = L'=')
    {
      const size_t separatorPosition = strUnparsed.find_first_of(L'=');
      if (std::wstring_view::npos == separatorPosition) return false;

      lhs = Strings::TrimWhitespace(strUnparsed.substr(0, separatorPosition));
      rhs = Strings::TrimWhitespace(strUnparsed.substr(1 + separatorPosition));
      return true;
    }

    /// Parses a directive and its parameters for the specified configuration file line, which must
    /// first have been classified as containing a directive.
    /// @param [in] configLineTrimmed Line read from the configuration file with whitespace already
    /// trimmed.
    /// @param [out] directiveString Filled with the directive itself.
    /// @param [out] maybeParamsString Filled with unparsed parameters to the directive, if they are
    /// present.
    static void ParseDirective(
        std::wstring_view configLineTrimmed,
        std::wstring_view& directiveString,
        std::optional<std::wstring_view>& maybeParamsString)
    {
      DebugAssert(
          0 == configLineTrimmed.find_first_of(L'%'),
          "Attempting to parse a directive from a line that does not contain one.");

      // Skip over the '%' sign and any whitespace after it but before the directive name.
      configLineTrimmed.remove_prefix(1);
      configLineTrimmed = Strings::TrimLeadingWhitespace(configLineTrimmed);

      // Directives are separated from their parameters by whitespace. An absence of whitespace
      // indicates an absence of parameters.
      const size_t firstSpacePosition = configLineTrimmed.find_first_of(L' ');
      if (std::wstring_view::npos == firstSpacePosition)
      {
        directiveString = configLineTrimmed;
        maybeParamsString = std::nullopt;
      }
      else
      {
        directiveString = Strings::TrimWhitespace(configLineTrimmed.substr(0, firstSpacePosition));
        maybeParamsString =
            Strings::TrimWhitespace(configLineTrimmed.substr(1 + firstSpacePosition));
      }
    }

    /// Parses a name and a value for the specified configuration file line, which must first
    /// have been classified as containing a name and value pair.
    /// @param [in] configLineTrimmed Line read from the configuration file with whitespace already
    /// trimmed.
    /// @param [out] nameString Filled with the name of the configuration setting.
    /// @param [out] valueString Filled with the value specified for the configuration setting.
    static void ParseNameAndValue(
        std::wstring_view configLineTrimmed,
        std::wstring_view& nameString,
        std::wstring_view& valueString)
    {
      const bool parseResult = ParseAssignmentString(configLineTrimmed, nameString, valueString);
      DebugAssert(
          true == parseResult,
          "Attempting to parse a name/value pair from a line that does not contain one.");
    }

    /// Parses a section name from the specified configuration file line, which must first have
    /// been classified as containing a section name.
    /// @param [in] configLineTrimmed Line read from the configuration file with whitespace already
    /// trimmed.
    /// @param String containing the name of the configuration section.
    static std::wstring_view ParseSection(std::wstring_view configLineTrimmed)
    {
      DebugAssert(
          ((std::wstring_view::npos != configLineTrimmed.find_first_of(L'[')) &&
           (std::wstring_view::npos != configLineTrimmed.find_first_of(L']')) &&
           (configLineTrimmed.find_first_of(L']') > configLineTrimmed.find_first_of(L'['))),
          "Attempting to parse a section name from a line that does not contain one.");

      std::wstring_view parsedSection = configLineTrimmed;
      parsedSection.remove_prefix(1 + configLineTrimmed.find_first_of(L'['));
      parsedSection.remove_suffix(
          configLineTrimmed.length() - configLineTrimmed.find_first_of(L']'));
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

    void ConfigurationFileReader::AppendErrorMessage(
        const SReadState& readState, std::wstring_view errorMessage)
    {
      if (false == errorMessages.has_value()) errorMessages.emplace();
      errorMessages->emplace_back(errorMessage);

      // If more than one reader is present in the stack, an "include backtrace" will additionally
      // be appended to the list of error messages, one per element in the stack.
      if (readState.readers.Size() >= 2)
      {
        constexpr wchar_t kContinuationArrow = L'\x21b3';
        int indentNumSpaces = 2;
        for (int64_t i = static_cast<int64_t>(readState.readers.Size()) - 2; i >= 0; --i)
        {
          const ConfigSourceReader& reader =
              *(readState.readers[static_cast<unsigned int>(i)].get());
          errorMessages->emplace_back(Strings::Format(
              L"%*s%c Included from %.*s(%u)",
              indentNumSpaces,
              L"",
              kContinuationArrow,
              static_cast<int>(reader.GetConfigSourceName().length()),
              reader.GetConfigSourceName().data(),
              reader.GetLastReadConfigLineNumber()));
          indentNumSpaces += 2;
        }
      }
    }

    ConfigurationData ConfigurationFileReader::ReadConfigurationFile(
        std::wstring_view configFileName, bool mustExist)
    {
      SReadState readState;

      errorMessages.reset();
      std::unique_ptr<ConfigSourceReader> fileReader =
          std::make_unique<FileReader>(configFileName.data());
      if (true == fileReader->IsError())
      {
        if (true == mustExist)
        {
          AppendErrorMessage(
              readState,
              Strings::Format(
                  L"%.*s: Unable to open file.",
                  static_cast<int>(configFileName.length()),
                  configFileName.data()));
        }
        return ConfigurationData();
      }

      readState.readers.EmplaceBack(std::move(fileReader));

      BeginRead();
      ReadConfigurationInternal(readState);
      EndRead();

      return std::move(readState.configToFill);
    }

    ConfigurationData ConfigurationFileReader::ReadInMemoryConfigurationFile(
        std::wstring_view configBuffer)
    {
      SReadState readState;
      readState.allowReadFromInMemoryConfigFiles = true;

      errorMessages.reset();
      std::unique_ptr<ConfigSourceReader> memoryBufferReader =
          std::make_unique<MemoryBufferReader>(configBuffer);
      readState.readers.EmplaceBack(std::move(memoryBufferReader));

      BeginRead();
      ReadConfigurationInternal(readState);
      EndRead();

      return std::move(readState.configToFill);
    }

    void ConfigurationFileReader::HandleIncludeDirective(
        SReadState& readState, std::wstring_view configFileToInclude)
    {
      const ConfigSourceReader& reader = readState.GetCurrentReader();
      std::unique_ptr<ConfigSourceReader> nextReader;

      constexpr std::wstring_view kInMemoryConfigFilePrefix = L"inmemory://";
      if ((true == readState.allowReadFromInMemoryConfigFiles) &&
          (true ==
           Strings::StartsWithCaseInsensitive<wchar_t>(
               configFileToInclude, kInMemoryConfigFilePrefix)))
      {
        configFileToInclude.remove_prefix(kInMemoryConfigFilePrefix.length());

        TIntegerValue pointerParsed = 0;
        const bool parsePointerResult =
            ParseTypedValue<TIntegerValue>(configFileToInclude, pointerParsed);
        if ((false == parsePointerResult) ||
            (static_cast<size_t>(parsePointerResult) > std::numeric_limits<size_t>::max()))
        {
          AppendErrorMessage(
              readState,
              Strings::Format(
                  L"%.*s(%u): %.*s: Unable to parse into a pointer.",
                  static_cast<int>(reader.GetConfigSourceName().length()),
                  reader.GetConfigSourceName().data(),
                  reader.GetLastReadConfigLineNumber(),
                  static_cast<int>(configFileToInclude.length()),
                  configFileToInclude.data()));
          return;
        }
        nextReader =
            std::make_unique<MemoryBufferReader>(reinterpret_cast<wchar_t*>(pointerParsed));
      }
      else
      {
        nextReader = std::make_unique<FileReader>(configFileToInclude);
        if (true == nextReader->IsError())
        {
          AppendErrorMessage(
              readState,
              Strings::Format(
                  L"%.*s(%u): %.*s: Unable to open file.",
                  static_cast<int>(reader.GetConfigSourceName().length()),
                  reader.GetConfigSourceName().data(),
                  reader.GetLastReadConfigLineNumber(),
                  static_cast<int>(configFileToInclude.length()),
                  configFileToInclude.data()));
          return;
        }
      }

      readState.readers.EmplaceBack(std::move(nextReader));
      ReadConfigurationInternal(readState);
      readState.readers.PopBack();
    }

    void ConfigurationFileReader::ParseAndMaybeHandleDirective(
        SReadState& readState, std::wstring_view configLineTrimmed)
    {
      const ConfigSourceReader& reader = readState.GetCurrentReader();

      std::wstring_view directiveString;
      std::optional<std::wstring_view> maybeParamsString;
      ParseDirective(configLineTrimmed, directiveString, maybeParamsString);

      if (Strings::EqualsCaseInsensitive<wchar_t>(directiveString, L"include"))
      {
        if (false == maybeParamsString.has_value())
        {
          AppendErrorMessage(
              readState,
              Strings::Format(
                  L"%.*s(%u): %.*s: Missing operand.",
                  static_cast<int>(reader.GetConfigSourceName().length()),
                  reader.GetConfigSourceName().data(),
                  reader.GetLastReadConfigLineNumber(),
                  static_cast<int>(directiveString.length()),
                  directiveString.data()));
          return;
        }
        HandleIncludeDirective(readState, *maybeParamsString);
      }
      else
      {
        AppendErrorMessage(
            readState,
            Strings::Format(
                L"%.*s(%u): %.*s: Unrecognized directive.",
                static_cast<int>(reader.GetConfigSourceName().length()),
                reader.GetConfigSourceName().data(),
                reader.GetLastReadConfigLineNumber(),
                static_cast<int>(directiveString.length()),
                directiveString.data()));
      }
    }

    void ConfigurationFileReader::ParseAndMaybeInsertSection(
        SReadState& readState, std::wstring_view configLineTrimmed)
    {
      const ConfigSourceReader& reader = readState.GetCurrentReader();

      std::wstring_view section = ParseSection(configLineTrimmed);
      if (0 != readState.seenSections.count(section))
      {
        AppendErrorMessage(
            readState,
            Strings::Format(
                L"%.*s(%u): %.*s: Duplicated section name.",
                static_cast<int>(reader.GetConfigSourceName().length()),
                reader.GetConfigSourceName().data(),
                reader.GetLastReadConfigLineNumber(),
                static_cast<int>(section.length()),
                section.data()));
        readState.skipValueLines = true;
        return;
      }

      const Action sectionAction = ActionForSection(section);
      switch (sectionAction.GetAction())
      {
        case EAction::Error:
          if (true == sectionAction.HasErrorMessage())
            AppendErrorMessage(
                readState,
                Strings::Format(
                    L"%.*s(%u): %s",
                    static_cast<int>(reader.GetConfigSourceName().length()),
                    reader.GetConfigSourceName().data(),
                    reader.GetLastReadConfigLineNumber(),
                    sectionAction.GetErrorMessage().c_str()));
          else
            AppendErrorMessage(
                readState,
                Strings::Format(
                    L"%.*s(%u): %.*s: Unrecognized section name.",
                    static_cast<int>(reader.GetConfigSourceName().length()),
                    reader.GetConfigSourceName().data(),
                    reader.GetLastReadConfigLineNumber(),
                    static_cast<int>(section.length()),
                    section.data()));
          readState.skipValueLines = true;
          break;

        case EAction::Process:
          readState.currentSection = *(readState.seenSections.emplace(section).first);
          readState.skipValueLines = false;
          break;

        case EAction::Skip:
          readState.skipValueLines = true;
          break;

        default:
          AppendErrorMessage(
              readState,
              Strings::Format(
                  L"%.*s(%u): Internal error while processing section name.",
                  static_cast<int>(reader.GetConfigSourceName().length()),
                  reader.GetConfigSourceName().data(),
                  reader.GetLastReadConfigLineNumber()));
          readState.skipValueLines = true;
          break;
      }
    }

    template <typename ValueType> void ConfigurationFileReader::ParseAndMaybeInsertValue(
        SReadState& readState,
        std::wstring_view name,
        EValueType valueType,
        std::wstring_view valueUnparsed)
    {
      const ConfigSourceReader& reader = readState.GetCurrentReader();

      ValueType valueParsed{};
      if (false == ParseTypedValue(valueUnparsed, valueParsed))
      {
        std::wstring_view typeString = TypeToString(ValueTypeToSingleValueEnumerator<ValueType>());
        AppendErrorMessage(
            readState,
            Strings::Format(
                L"%.*s(%u): %.*s: Failed to parse %.*s value.",
                static_cast<int>(reader.GetConfigSourceName().length()),
                reader.GetConfigSourceName().data(),
                reader.GetLastReadConfigLineNumber(),
                static_cast<int>(valueUnparsed.length()),
                valueUnparsed.data(),
                static_cast<int>(typeString.length()),
                typeString.data()));
        return;
      }

      const Action valueAction = ActionForValue(readState.currentSection, name, valueParsed);
      switch (valueAction.GetAction())
      {
        case EAction::Error:
          if (true == valueAction.HasErrorMessage())
            AppendErrorMessage(
                readState,
                Strings::Format(
                    L"%.*s(%u): %s",
                    static_cast<int>(reader.GetConfigSourceName().length()),
                    reader.GetConfigSourceName().data(),
                    reader.GetLastReadConfigLineNumber(),
                    valueAction.GetErrorMessage().c_str()));
          else
            AppendErrorMessage(
                readState,
                Strings::Format(
                    L"%.*s(%u): %.*s: Invalid value for configuration setting %.*s.",
                    static_cast<int>(reader.GetConfigSourceName().length()),
                    reader.GetConfigSourceName().data(),
                    reader.GetLastReadConfigLineNumber(),
                    static_cast<int>(valueUnparsed.length()),
                    valueUnparsed.data(),
                    static_cast<int>(name.length()),
                    name.data()));
          break;

        case EAction::Process:
          if (false ==
              readState.configToFill.Insert(
                  readState.currentSection,
                  name,
                  Value(
                      std::move(valueParsed),
                      valueType,
                      reader.GetConfigSourceName(),
                      reader.GetLastReadConfigLineNumber())))
            AppendErrorMessage(
                readState,
                Strings::Format(
                    L"%.*s(%u): %.*s: Duplicated value for configuration setting %.*s.",
                    static_cast<int>(reader.GetConfigSourceName().length()),
                    reader.GetConfigSourceName().data(),
                    reader.GetLastReadConfigLineNumber(),
                    static_cast<int>(valueUnparsed.length()),
                    valueUnparsed.data(),
                    static_cast<int>(name.length()),
                    name.data()));
          break;
      }
    }

    void ConfigurationFileReader::ReadConfigurationInternal(SReadState& readState)
    {
      ConfigSourceReader& reader = readState.GetCurrentReader();

      TemporaryString configLine;
      bool configLineReadResult = reader.ReadLine(configLine);

      while (true == configLineReadResult)
      {
        std::wstring_view configLineTrimmed = Strings::TrimWhitespace(configLine.AsStringView());

        switch (ClassifyConfigurationFileLine(configLineTrimmed))
        {
          case ELineClassification::Error:
            AppendErrorMessage(
                readState,
                Strings::Format(
                    L"%.*s(%u): Unable to parse line.",
                    static_cast<int>(reader.GetConfigSourceName().length()),
                    reader.GetConfigSourceName().data(),
                    reader.GetLastReadConfigLineNumber()));
            break;

          case ELineClassification::Directive:
            ParseAndMaybeHandleDirective(readState, configLineTrimmed);
            break;

          case ELineClassification::Ignore:
            break;

          case ELineClassification::Section:
            ParseAndMaybeInsertSection(readState, configLineTrimmed);
            break;

          case ELineClassification::Value:
            if (false == readState.skipValueLines)
            {
              std::wstring_view name;
              std::wstring_view value;
              ParseNameAndValue(configLineTrimmed, name, value);

              const auto& existingName = readState.configToFill[readState.currentSection][name];
              const EValueType valueType =
                  (existingName.HasValue() ? existingName.GetType()
                                           : TypeForValue(readState.currentSection, name));

              // If the value type does not identify it as multi-valued, make sure
              // this is the first time the setting is seen.
              if ((false == TypeAllowsMultipleValues(valueType)) &&
                  (readState.configToFill.Contains(readState.currentSection, name)))
              {
                AppendErrorMessage(
                    readState,
                    Strings::Format(
                        L"%.*s(%u): %.*s: Only a single value is allowed for this setting.",
                        static_cast<int>(reader.GetConfigSourceName().length()),
                        reader.GetConfigSourceName().data(),
                        reader.GetLastReadConfigLineNumber(),
                        static_cast<int>(name.length()),
                        name.data()));
                break;
              }

              switch (valueType)
              {
                case EValueType::Error:
                  AppendErrorMessage(
                      readState,
                      Strings::Format(
                          L"%.*s(%u): %.*s: Unrecognized configuration setting.",
                          static_cast<int>(reader.GetConfigSourceName().length()),
                          reader.GetConfigSourceName().data(),
                          reader.GetLastReadConfigLineNumber(),
                          static_cast<int>(name.length()),
                          name.data()));
                  break;

                case EValueType::Integer:
                case EValueType::IntegerMultiValue:
                  ParseAndMaybeInsertValue<TIntegerValue>(readState, name, valueType, value);
                  break;

                case EValueType::Boolean:
                case EValueType::BooleanMultiValue:
                  ParseAndMaybeInsertValue<TBooleanValue>(readState, name, valueType, value);
                  break;

                case EValueType::String:
                case EValueType::StringMultiValue:
                  ParseAndMaybeInsertValue<TStringValue>(readState, name, valueType, value);
                  break;

                default:
                  AppendErrorMessage(
                      readState,
                      Strings::Format(
                          L"%.*s(%u): Internal error while processing configuration setting.",
                          static_cast<int>(reader.GetConfigSourceName().length()),
                          reader.GetConfigSourceName().data(),
                          reader.GetLastReadConfigLineNumber()));
                  break;
              }
            }
            break;

          default:
            AppendErrorMessage(
                readState,
                Strings::Format(
                    L"%.*s(%u): Internal error while processing line.",
                    static_cast<int>(reader.GetConfigSourceName().length()),
                    reader.GetConfigSourceName().data(),
                    reader.GetLastReadConfigLineNumber()));
            break;
        }

        configLineReadResult = reader.ReadLine(configLine);
      }

      if (false == reader.IsEndOfInput())
      {
        // Stopped reading the configuration file early due to some condition other than
        // end-of-file. This indicates an error.

        if (true == reader.IsError())
        {
          AppendErrorMessage(
              readState,
              Strings::Format(
                  L"%.*s(%u): I/O error while reading.",
                  static_cast<int>(reader.GetConfigSourceName().length()),
                  reader.GetConfigSourceName().data(),
                  reader.GetLastReadConfigLineNumber()));
          return;
        }
        else if (false == configLineReadResult)
        {
          AppendErrorMessage(
              readState,
              Strings::Format(
                  L"%.*s(%u): Line is too long.",
                  static_cast<int>(reader.GetConfigSourceName().length()),
                  reader.GetConfigSourceName().data(),
                  reader.GetLastReadConfigLineNumber()));
          return;
        }
      }
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
