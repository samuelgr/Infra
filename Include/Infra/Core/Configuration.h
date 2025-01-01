/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file Configuration.h
 *   Declaration of configuration file functionality.
 **************************************************************************************************/

#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

#include "DebugAssert.h"
#include "Message.h"
#include "Strings.h"
#include "ValueOrError.h"

/// Convenience wrapper around initializer list syntax for defining a configuration file section in
/// a layout object. Specify a section name followed by a series of setting name and value type
/// pairs.
#define ConfigurationFileLayoutSection(section, ...)                                               \
  {                                                                                                \
    (section),                                                                                     \
    {                                                                                              \
      __VA_ARGS__                                                                                  \
    }                                                                                              \
  }

/// Convenience wrapper around initializer list syntax for defining a setting name and value type
/// pair. Intended for use within the initializer for a configuration file section layout.
#define ConfigurationFileLayoutNameAndValueType(name, valueType)                                   \
  {                                                                                                \
    (name), (valueType)                                                                            \
  }

namespace Infra
{
  namespace Configuration
  {
    /// Generates and returns the recommended file path for a configuration file. This is just the
    /// product name plus the .ini extension located in the same directory as this module.
    /// @return Recommended configuration filename.
    std::wstring_view RecommendedConfigurationFilePath(void);

    /// Section name for all settings that appear at global scope (i.e. outside of a section).
    inline constexpr std::wstring_view kSectionNameGlobal = L"";

    /// Enumerates possible directives that can be issued in response to a query on how to
    /// process a section or a name/value pair encountered in a configuration file.
    enum class EAction
    {
      /// Flag an error. For sections, this means the remainder of the section is skipped.
      Error,

      /// Continue processing. For sections this means the name/value pairs within will be
      /// read. For name/value pairs this means the pair will be inserted into the
      /// configuration data structure.
      Process,

      /// Skip. For sections this means to ignore all the name/value pairs within. For
      /// name/value pairs this means to do nothing.
      Skip,
    };

    /// Enumerates all supported types for configuration values.
    /// Used when checking with a subclass for guidance on whether a section/name pair is
    /// supported and, if so, how to parse the value.
    enum class EValueType
    {
      /// Combination of section and name pair is not supported.
      Error,

      /// Combination of section and name pair is supported; value is a single integer.
      Integer,

      /// Combination of section and name pair is supported; value is a single Boolean.
      Boolean,

      /// Combination of section and name pair is supported; value is a single string.
      String,

      /// Combination of section and name pair is supported; value is integer and multiple
      /// values are allowed.
      IntegerMultiValue,

      /// Combination of section and name pair is supported; value is Boolean and multiple
      /// values are allowed.
      BooleanMultiValue,

      /// Combination of section and name pair is supported; value is string and multiple
      /// values are allowed.
      StringMultiValue,
    };

    /// Fully defines an action to take in response to a section or a value being read. Combines an
    /// action with a possible error message.
    class Action
    {
    public:

      bool operator==(const Action& other) const = default;

      /// Creates an action that specifies to process the section or value into the configuration
      /// data object.
      /// @return Action that says to process the current line.
      static inline Action Process(void)
      {
        return Action(EAction::Process);
      }

      /// Creates an action that specifies to skip the section or value. It will not be inserted
      /// into the configuration data object.
      /// @return Action that says to skip the current line.
      static inline Action Skip(void)
      {
        return Action(EAction::Skip);
      }

      /// Creates an action that specifies to raise an error for the section or value but without
      /// specifying a reason. It will not be inserted into the configuration data object.
      /// @return Action that says to raise an error for the current line.
      static inline Action Error(void)
      {
        return Action(EAction::Error);
      }

      /// Creates an action that specifies to raise an error for the section or value. It will not
      /// be inserted into the configuration data object and either the specified error message or a
      /// default error message will be produced.
      /// @return Action that says to raise an error for the current line.
      template <typename StringType>
        requires IsStringType<StringType>
      static inline Action ErrorWithMessage(StringType errorMessage)
      {
        return Action(EAction::Error, std::wstring(errorMessage));
      }

      /// Retrieves the action enumerator specified by this object.
      /// @return Action enumerator.
      inline EAction GetAction(void) const
      {
        return action;
      }

      /// Retrieves a read-only view of the error message specified by this object.
      /// @return View of the error message.
      inline const std::wstring& GetErrorMessage(void) const
      {
        return errorMessage;
      }

      /// Determines whether or not this object contains an error message.
      /// @return `true` if so, `false` otherwise.
      inline bool HasErrorMessage(void) const
      {
        return (false == errorMessage.empty());
      }

    private:

      Action(EAction action, std::wstring&& errorMessage = std::wstring())
          : action(action), errorMessage(std::move(errorMessage))
      {}

      /// Action to be taken. One of the enumerators.
      EAction action;

      /// Error message. Only filled in if there is an error.
      std::wstring errorMessage;
    };

    /// Underlying type used for storing integer-typed values.
    using TIntegerValue = int64_t;

    /// Underlying type used for storing Boolean-valued types.
    using TBooleanValue = bool;

    /// Underlying type used for storing string-valued types.
    using TStringValue = std::wstring;

    /// View type used for retrieving and returning integer-typed values.
    using TIntegerView = TIntegerValue;

    /// View type used for retrieving and returning integer-typed values.
    using TBooleanView = TBooleanValue;

    /// View type used for retrieving and returning string-typed values.
    using TStringView = std::wstring_view;

    /// Determines if the specified type is an integer type, with Boolean type explicitly excluded.
    template <typename MaybeIntegerType> concept IsIntegerType = std::conjunction_v<
        std::is_integral<MaybeIntegerType>,
        std::negation<std::is_same<MaybeIntegerType, bool>>>;

    /// Determines if the specified type is exactly Boolean.
    template <typename MaybeBooleanType> concept IsBooleanType =
        std::is_same_v<MaybeBooleanType, bool>;

    /// Determines if the specified type is a string type.
    template <typename MaybeStringType> concept IsStringType =
        std::is_convertible_v<MaybeStringType, TStringView>;

    /// Fourth-level object used to represent a single configuration value for a particular
    /// configuration setting. String values are case-sensitive.
    class Value
    {
    public:

      /// Creates an integer-typed value.
      template <typename IntegerType>
        requires IsIntegerType<IntegerType>
      inline Value(
          IntegerType value,
          EValueType type = EValueType::Integer,
          std::wstring_view configSourceName = std::wstring_view(),
          int configSourceLineNumber = 0)
          : configSourceName(configSourceName),
            configSourceLineNumber(configSourceLineNumber),
            type(type),
            intValue(static_cast<TIntegerValue>(value))
      {
        DebugAssert(TypeIsInteger(), "Mismatch between value and type enumerator.");
      }

      /// Creates a Boolean-typed value.
      template <typename BooleanType>
        requires IsBooleanType<BooleanType>
      inline Value(
          BooleanType value,
          EValueType type = EValueType::Boolean,
          std::wstring_view configSourceName = std::wstring_view(),
          int configSourceLineNumber = 0)
          : configSourceName(configSourceName),
            configSourceLineNumber(configSourceLineNumber),
            type(type),
            boolValue(value)
      {
        DebugAssert(TypeIsBoolean(), "Mismatch between value and type enumerator.");
      }

      /// Creates a string-typed value by copying it.
      template <typename StringType>
        requires IsStringType<StringType>
      inline Value(
          StringType value,
          EValueType type = EValueType::String,
          std::wstring_view configSourceName = std::wstring_view(),
          int configSourceLineNumber = 0)
          : configSourceName(configSourceName),
            configSourceLineNumber(configSourceLineNumber),
            type(type),
            stringValue(value)
      {
        DebugAssert(TypeIsString(), "Mismatch between value and type enumerator.");
      }

      /// Creates a string-typed value by moving it.
      inline Value(
          TStringValue&& value,
          EValueType type = EValueType::String,
          std::wstring_view configSourceName = std::wstring_view(),
          int configSourceLineNumber = 0)
          : configSourceName(configSourceName),
            configSourceLineNumber(configSourceLineNumber),
            type(type),
            stringValue(std::move(value))
      {
        DebugAssert(TypeIsString(), "Mismatch between value and type enumerator.");
      }

      Value(const Value& other);

      Value(Value&& other) noexcept;

      ~Value(void);

      bool operator<(const Value& rhs) const;

      bool operator==(const Value& rhs) const;

      inline operator TIntegerView(void) const
      {
        DebugAssert(true == TypeIsInteger(), "Object does not hold an integer value.");
        return intValue;
      }

      inline operator TBooleanView(void) const
      {
        DebugAssert(true == TypeIsBoolean(), "Object does not hold a Boolean value.");
        return boolValue;
      }

      inline operator TStringView(void) const
      {
        DebugAssert(true == TypeIsString(), "Object does not hold a string value.");
        return stringValue;
      }

      /// Enables equality comparison with an integer value or literal.
      template <typename IntegerType>
        requires IsIntegerType<IntegerType>
      inline bool operator==(IntegerType rhs) const
      {
        return (TypeIsInteger() && (static_cast<TIntegerView>(rhs) == GetInteger()));
      }

      /// Enables equality comparison with a Boolean value or literal.
      template <typename BooleanType>
        requires IsBooleanType<BooleanType>
      inline bool operator==(BooleanType rhs) const
      {
        return (TypeIsBoolean() && (static_cast<TBooleanView>(rhs) == GetBoolean()));
      }

      /// Enables equality comparison with a string value or literal.
      template <typename StringType>
        requires IsStringType<StringType>
      inline bool operator==(const StringType& rhs) const
      {
        return (TypeIsString() && (TStringView(rhs) == GetString()));
      }

      /// Determines whether the type of value held by this object matches the template
      /// parameter.
      /// @tparam ValueType Type of value to check against what is held by this object.
      /// @return `true` if the template parameter value matches the actual value type held by
      /// this object, `false` otherwise.
      template <typename ValueType> bool TypeIs(void) const;

      /// Determines whether or not the type of value held by this object is integer.
      /// @return `true` if so, `false` otherwise.
      bool TypeIsInteger(void) const;

      /// Determines whether or not the type of value held by this object is Boolean.
      /// @return `true` if so, `false` otherwise.
      bool TypeIsBoolean(void) const;

      /// Determines whether or not the type of value held by this object is string.
      /// @return `true` if so, `false` otherwise.
      bool TypeIsString(void) const;

      /// Extracts the value held by this object without checking for type-correctness and
      /// returns it using move semantics.
      /// @tparam ValueType Type of value to extract from this object.
      /// @return Value extracted from this object.
      template <typename ValueType> ValueType Extract(void);

      /// Extracts the value held by this object and assumes it is integer type.
      /// @return Extracted value.
      TIntegerValue ExtractInteger(void);

      /// Extracts the value held by this object and assumes it is Boolean type.
      /// @return Extracted value.
      TBooleanValue ExtractBoolean(void);

      /// Extracts the value held by this object and assumes it is string type.
      /// @return Extracted value.
      TStringValue ExtractString(void);

      /// Retrieves and returns the name of the configuration source from which this value was
      /// obtained.
      /// @return Name of the configuration source from which this value was obtained.
      inline std::wstring_view GetConfigSourceName(void) const
      {
        return configSourceName;
      }

      /// Retrieves and returns the line number within the configuration source on which this
      /// value was located.
      /// @return Line number that corresponds to this value.
      inline int GetConfigSourceLineNumber(void) const
      {
        return configSourceLineNumber;
      }

      /// Retrieves and returns the type of the stored value.
      /// @return Type of the stored value.
      inline EValueType GetType(void) const
      {
        return type;
      }

      /// Retrieves and returns an immutable view of the stored value without checking for
      /// type-correctness.
      /// @tparam ViewType Type of view to retrieve from this object.
      /// @return View of the stored value.
      template <typename ViewType> ViewType Get(void) const;

      /// Retrieves and returns the stored value, which is assumed to be integer type.
      /// @return Stored value.
      TIntegerView GetInteger(void) const;

      /// Retrieves and returns the stored value, which is assumed to be Boolean type.
      /// @return Stored value.
      TBooleanView GetBoolean(void) const;

      /// Retrieves and returns a read-only view of the stored value, which is assumed to be string
      /// type.
      /// @return Read-only view of the stored value.
      TStringView GetString(void) const;

    private:

      /// Name of the configuration file on which this value was located.
      std::wstring_view configSourceName;

      /// Line number within the configuration file on which this value was located.
      int configSourceLineNumber;

      /// Indicates the value type.
      EValueType type;

      /// Holds the value itself.
      union
      {
        TIntegerValue intValue;
        TBooleanValue boolValue;
        TStringValue stringValue;
      };
    };

    /// Third-level object used to represent a single configuration setting within one section
    /// of a configuration file.
    class Name
    {
    public:

      /// Alias for the underlying data structure used to store per-setting configuration
      /// values. Comparisons are delegated to whatever the #Value class supports.
      using TValues = std::set<Value, std::less<>>;

      Name(void) = default;

      /// Allows contents to be specified directly using multiple integer literals. Intended
      /// for use by tests.
      template <typename IntegerType>
        requires IsIntegerType<IntegerType>
      inline Name(std::initializer_list<IntegerType> values) : Name()
      {
        for (const auto& value : values)
          Insert(Value(
              static_cast<TIntegerView>(value),
              ((values.size() > 1) ? EValueType::IntegerMultiValue : EValueType::Integer)));
      }

      /// Allows contents to be specified directly using a single integer literal. Intended
      /// for use by tests.
      template <typename IntegerType>
        requires IsIntegerType<IntegerType>
      inline Name(IntegerType value) : Name({value})
      {}

      /// Allows contents to be specified directly using multiple Boolean literals. Intended
      /// for use by tests.
      template <typename BooleanType>
        requires IsBooleanType<BooleanType>
      inline Name(std::initializer_list<BooleanType> values) : Name()
      {
        for (const auto& value : values)
          Insert(Value(
              static_cast<TBooleanView>(value),
              ((values.size() > 1) ? EValueType::BooleanMultiValue : EValueType::Boolean)));
      }

      /// Allows contents to be specified directly using a single Boolean literal. Intended
      /// for use by tests.
      template <typename BooleanType>
        requires IsBooleanType<BooleanType>
      inline Name(BooleanType value) : Name({value})
      {}

      /// Allows contents to be specified directly using multiple string literals. Intended
      /// for use by tests.
      template <typename StringType>
        requires IsStringType<StringType>
      inline Name(std::initializer_list<StringType> values) : Name()
      {
        for (const auto& value : values)
          Insert(Value(
              TStringView(value),
              ((values.size() > 1) ? EValueType::StringMultiValue : EValueType::String)));
      }

      /// Allows contents to be specified directly using a single string literal. Intended for
      /// use by tests.
      template <typename StringType>
        requires IsStringType<StringType>
      inline Name(StringType value) : Name({value})
      {}

      bool operator==(const Name& rhs) const = default;

      inline const Value* operator->(void) const
      {
        return &(*values.begin());
      }

      inline const Value& operator*(void) const
      {
        return *values.begin();
      }

      /// Extracts the first value from this configruation setting using move semantics.
      /// @tparam ValueType Expected value type of the configuration setting to extract.
      /// @return Extracted value if the value type matches the template parameter.
      template <typename ValueType> std::optional<ValueType> ExtractFirst(void);

      /// Extracts the first Boolean value from this configuration setting using move
      /// semantics.
      /// @return Extracted value if the value is of type Boolean.
      inline std::optional<TBooleanValue> ExtractFirstBoolean(void)
      {
        return ExtractFirst<TBooleanValue>();
      }

      /// Extracts the first integer value from this configuration setting using move
      /// semantics.
      /// @return Extracted value if the value is of type integer.
      inline std::optional<TIntegerValue> ExtractFirstInteger(void)
      {
        return ExtractFirst<TIntegerValue>();
      }

      /// Extracts the first string value from this configuration setting using move
      /// semantics.
      /// @return Extracted value if the value is of type string.
      inline std::optional<TStringValue> ExtractFirstString(void)
      {
        return ExtractFirst<TStringValue>();
      }

      /// Extracts all values from this configuration setting using move semantics and returns
      /// them as a vector.
      /// @tparam ValueType Expected value type of the configuration setting to extract.
      /// @return Vector of extracted values if the value type matches the template parameter.
      template <typename ValueType> std::optional<std::vector<ValueType>> ExtractAll(void);

      /// Extracts all Boolean values from this configuration setting using move semantics and
      /// returns them as a vector.
      /// @return Vector of extracted Boolean values if the they are of type Boolean.
      inline std::optional<std::vector<TBooleanValue>> ExtractAllBooleans(void)
      {
        return ExtractAll<TBooleanValue>();
      }

      /// Extracts all integer values from this configuration setting using move semantics and
      /// returns them as a vector.
      /// @return Vector of extracted Boolean values if the they are of type integer.
      inline std::optional<std::vector<TIntegerValue>> ExtractAllIntegers(void)
      {
        return ExtractAll<TIntegerValue>();
      }

      /// Extracts all string values from this configuration setting using move semantics and
      /// returns them as a vector.
      /// @return Vector of extracted Boolean values if the they are of type string.
      inline std::optional<std::vector<TStringValue>> ExtractAllStrings(void)
      {
        return ExtractAll<TStringValue>();
      }

      /// Allows read-only access to the first stored value. Useful for single-valued settings.
      /// @return First stored value.
      inline const Value& GetFirst(void) const
      {
        return *(*this);
      }

      /// Retrieves and returns the type of the stored values for this configuration setting.
      /// If this configuration setting is empty by virtue of all values being extracted then
      /// it is considered typeless, so operations that attempt to get the type return
      /// error-type.
      /// @return Type of the stored values.
      inline EValueType GetType(void) const
      {
        if (Count() < 1) return EValueType::Error;
        return GetFirst().GetType();
      }

      /// Stores a new value for the configuration setting represented by this object by
      /// moving the input parameter. Will fail if the value already exists or if there is a type
      /// mismatch between the value being inserted and the values already present.
      /// @param [in] value Value to insert.
      /// @return `true` on success, `false` on failure.
      inline bool Insert(Value&& value)
      {
        if ((Count() >= 1) && (GetType() != value.GetType())) return false;
        return values.emplace(std::move(value)).second;
      }

      /// Determines if this configuration setting object is empty (i.e. contains no
      /// configuration data).
      /// @return `true` if so, `false` otherwise.
      inline bool Empty(void) const
      {
        return values.empty();
      }

      /// Determines if this configuration setting object has any values (i.e. is not empty).
      /// @return `true` if so, `false` otherwise.
      inline bool HasValue(void) const
      {
        return (false == Empty());
      }

      /// Determines whether the type of value held in this configuration setting matches the
      /// template parameter. If this configuration setting is empty by virtue of all values
      /// being extracted then it is considered typeless, so this method always returns
      /// `false`.
      /// @tparam ValueType Type of value to check against what is held by this configuration
      /// setting.
      /// @return `true` if the template parameter value matches the actual value type held by
      /// this object, `false` otherwise.
      template <typename ValueType> inline bool TypeIs(void) const
      {
        if (Count() < 1) return false;
        return GetFirst().TypeIs<ValueType>();
      }

      /// Determines whether or not the type of value held by this configuration setting is integer.
      /// @return `true` if so, `false` otherwise.
      inline bool TypeIsInteger(void) const
      {
        return TypeIs<TIntegerValue>();
      }

      /// Determines whether or not the type of value held by this configuration setting is Boolean.
      /// @return `true` if so, `false` otherwise.
      bool TypeIsBoolean(void) const
      {
        return TypeIs<TBooleanValue>();
      }

      /// Determines whether or not the type of value held by this configuration setting is string.
      /// @return `true` if so, `false` otherwise.
      bool TypeIsString(void) const
      {
        return TypeIs<TStringValue>();
      }

      /// Retrieves the number of values present for the configuration setting represented by
      /// this object.
      /// @return Number of values present.
      inline size_t Count(void) const
      {
        return values.size();
      }

      /// Retrieves the first integer value, if this object is non-empty and contains one or more
      /// integer values. Otherwise returns the specified default value instead.
      /// @param [in] defaultValue Value to return if this object is empty or the wrong type.
      /// @return First stored value or the specified default value.
      template <typename IntegerType>
        requires IsIntegerType<IntegerType>
      inline TIntegerView ValueOr(IntegerType defaultValue) const
      {
        if (true == TypeIsInteger()) return GetFirst().GetInteger();
        return defaultValue;
      }

      /// Retrieves the first Boolean value, if this object is non-empty and contains one or more
      /// Boolean values. Otherwise returns the specified default value instead.
      /// @param [in] defaultValue Value to return if this object is empty or the wrong type.
      /// @return First stored value or the specified default value.
      template <typename BooleanType>
        requires IsBooleanType<BooleanType>
      inline TBooleanView ValueOr(BooleanType defaultValue) const
      {
        if (true == TypeIsBoolean()) return GetFirst().GetBoolean();
        return defaultValue;
      }

      /// Retrieves the first string value, if this object is non-empty and contains one or more
      /// string values. Otherwise returns the specified default value instead.
      /// @param [in] defaultValue Value to return if this object is empty or the wrong type.
      /// @return First stored value or the specified default value.
      template <typename StringType>
        requires IsStringType<StringType>
      inline TStringView ValueOr(StringType defaultValue) const
      {
        if (true == TypeIsString()) return GetFirst().GetString();
        return TStringView(defaultValue);
      }

      /// Allows read-only access to all values. Useful for iterating.
      /// @return Container of all values.
      inline const TValues& Values(void) const
      {
        return values;
      }

    private:

      /// Holds all values for each configuration setting, one element per value.
      TValues values;
    };

    /// Second-level object used to represent an entire section of a configuration file.
    class Section
    {
      /// Alias for the underlying data structure used to store per-section configuration
      /// settings. Names of configuration settings are case-insensitive.
      using TNames =
          std::map<std::wstring, Name, Strings::CaseInsensitiveLessThanComparator<wchar_t>>;

    public:

      Section(void) = default;

      /// Allows contents to be specified directly. Intended for use by tests.
      inline Section(std::initializer_list<std::pair<std::wstring, Name>> contents) : Section()
      {
        for (const auto& name : contents)
          names.emplace(name);
      }

      bool operator==(const Section& rhs) const = default;

      /// Allows read-only access to individual configuration settings by name. Returns an empty
      /// object if the specified name does not exist.
      inline const Name& operator[](std::wstring_view name) const
      {
        static const Name kEmptyName;
        auto namesIter = names.find(name);
        return ((names.cend() == namesIter) ? kEmptyName : namesIter->second);
      }

      /// Determines if a configuration setting of the specified name exists in the section
      /// represented by this object.
      /// @param [in] name Name of the configuration setting to check.
      /// @return `true` if the setting exists, `false` otherwise.
      inline bool Contains(std::wstring_view name) const
      {
        return names.contains(name);
      }

      /// Retrieves the number of configuration settings present for the section represented
      /// by this object.
      /// @return Number of configuration settings present.
      inline size_t Count(void) const
      {
        return names.size();
      }

      /// Extracts the entire configuration setting of the specified name from this section using
      /// move semantics.
      /// @param [in] name Name of the configuration setting to extract.
      /// @return Extracted configuration setting, including all its values, if a setting of the
      /// specified name exists in this section.
      std::optional<Name> Extract(std::wstring_view name);

      /// Extracts the entire first configuration setting from this section using move
      /// semantics.
      /// @return Pair consisting of the configuration setting name and configuration setting
      /// object if this section is non-empty.
      std::optional<std::pair<std::wstring, Name>> ExtractFirst(void);

      /// Allows read-only access to the first stored configuration setting.
      /// @return First stored configuration setting.
      inline TNames::const_iterator GetFirst(void) const
      {
        return names.begin();
      }

      /// Stores a new value for the specified configuration setting in the section
      /// represented by this object by moving the input parameter. Will fail if the value
      /// already exists.
      /// @param [in] name Name of the configuration setting into which to insert the value.
      /// @param [in] value Value to insert.
      /// @return `true` on success, `false` on failure.
      inline bool Insert(std::wstring_view name, Value&& value)
      {
        auto nameIterator = names.find(name);
        if (names.end() == nameIterator)
          nameIterator = names.emplace(std::wstring(name), Name()).first;
        return nameIterator->second.Insert(std::move(value));
      }

      /// Determines if this configuration section data object is empty (i.e. contains no
      /// configuration data).
      /// @return `true` if so, `false` otherwise.
      inline bool Empty(void) const
      {
        return names.empty();
      }

      /// Allows read-only access to all configuration settings. Useful for iterating.
      /// @return Container of all configuration settings.
      inline const TNames& Names(void) const
      {
        return names;
      }

    private:

      /// Holds configuration data within each section, one element per configuration setting.
      TNames names;
    };

    /// Top-level object used to represent all configuration data read from a configuration
    /// file.
    class ConfigurationData
    {
    public:

      /// Alias for the underlying data structure used to hold all configuration source names that
      /// were read in creating this configuration data object. Names are all case-insensitive.
      using TConfigSourceNames =
          std::set<std::wstring, Strings::CaseInsensitiveLessThanComparator<wchar_t>>;

      /// Alias for the underlying data structure used to store top-level configuration
      /// section data. Names of sections are case-insensitive.
      using TSections =
          std::map<std::wstring, Section, Strings::CaseInsensitiveLessThanComparator<wchar_t>>;

      ConfigurationData(void) = default;

      /// Allows contents to be specified directly. Intended for use by tests.
      inline ConfigurationData(std::initializer_list<std::pair<std::wstring, Section>> contents)
          : ConfigurationData()
      {
        for (const auto& section : contents)
          sections.insert(section);
      }

      /// Allows read-only access to individual sections by name. Returns an empty object if the
      /// specified section does not exist.
      inline const Section& operator[](std::wstring_view section) const
      {
        static const Section kEmptySection;
        auto sectionsIter = sections.find(section);
        return ((sections.cend() == sectionsIter) ? kEmptySection : sectionsIter->second);
      }

      bool operator==(const ConfigurationData& rhs) const;

      /// Clears all of the stored configuration data.
      inline void Clear(void)
      {
        sections.clear();
      }

      /// Determines if a section of the specified name exists in the configuration
      /// represented by this object.
      /// @param [in] section Section name to check.
      /// @return `true` if the setting exists, `false` otherwise.
      inline bool Contains(std::wstring_view section) const
      {
        return sections.contains(section);
      }

      /// Determines if a section of the specified name exists in the configuration and, if so,
      /// whether or not it contains a configuration setting of the specified name.
      /// @param [in] section Section name to check.
      /// @param [in] name Name of the configuration setting to check.
      /// @return `true` if the setting exists, `false` otherwise.
      inline bool Contains(std::wstring_view section, std::wstring_view name) const
      {
        return (Contains(section) && (*this)[section].Contains(name));
      }

      /// Retrieves the number of sections present in the configuration represented by this
      /// object.
      /// @return Number of configuration settings present.
      inline size_t Count(void) const
      {
        return sections.size();
      }

      /// Extracts the specified section from this configuration data object using move
      /// semantics. This has the additional effect of erasing it from this configuration data
      /// object.
      /// @param [in] position Iterator that corresponds to the specific section object to be
      /// extracted.
      /// @return Pair containing the extracted section name and extracted section object.
      std::pair<std::wstring, Section> ExtractSection(TSections::const_iterator position);

      /// Extracts the specified section from this configuration data object using move
      /// semantics. This has the additional effect of erasing it from this configuration data
      /// object.
      /// @param [in] section Section name to extract.
      /// @return Pair containing the extracted section name and extracted section object, if
      /// the section was successfully located.
      inline std::optional<std::pair<std::wstring, Section>> ExtractSection(
          std::wstring_view section)
      {
        auto sectionIterator = sections.find(section);
        if (sections.end() == sectionIterator) return std::nullopt;
        return ExtractSection(sectionIterator);
      }

      /// Stores a new value for the specified configuration setting in the specified
      /// section. Will fail if the value already exists.
      /// @param [in] section Section into which to insert the configuration setting.
      /// @param [in] name Name of the configuration setting into which to insert the value.
      /// @param [in] value Value to insert.
      /// @param [in] valueType Type enumerator for the value to be parsed and possibly inserted.
      /// This is a separate parameter because it additionally allows single- or multi-valued to be
      /// specified. Defaults to the correct single-valued enumerator for this overload.
      /// @param [in] configSourceName Name of the configuration data source, such as a
      /// configuration file. Defaults to empty.
      /// @param [in] configSourceLineNumber Line number within the configuration data source of the
      /// value being inserted. Defaults to 0.
      /// @return `true` on success, `false` on failure.
      template <typename IntegerType>
        requires IsIntegerType<IntegerType>
      inline bool Insert(
          std::wstring_view section,
          std::wstring_view name,
          IntegerType intValue,
          EValueType valueType = EValueType::Integer,
          std::wstring_view configSourceName = L"",
          int configSourceLineNumber = 0)
      {
        return InsertInternal(
            section,
            name,
            Value(
                static_cast<TIntegerValue>(intValue),
                valueType,
                *(configSourceNames.emplace(configSourceName).first),
                configSourceLineNumber));
      }

      /// Stores a new Boolean value for the specified configuration setting in the specified
      /// section. Will fail if the value already exists.
      /// @param [in] section Section into which to insert the configuration setting.
      /// @param [in] name Name of the configuration setting into which to insert the value.
      /// @param [in] boolValue Value to insert.
      /// @param [in] valueType Type enumerator for the value to be parsed and possibly inserted.
      /// This is a separate parameter because it additionally allows single- or multi-valued to be
      /// specified. Defaults to the correct single-valued enumerator for this overload.
      /// @param [in] configSourceName Name of the configuration data source, such as a
      /// configuration file. Defaults to empty.
      /// @param [in] configSourceLineNumber Line number within the configuration data source of the
      /// value being inserted. Defaults to 0.
      /// @return `true` on success, `false` on failure.
      template <typename BooleanType>
        requires IsBooleanType<BooleanType>
      inline bool Insert(
          std::wstring_view section,
          std::wstring_view name,
          BooleanType boolValue,
          EValueType valueType = EValueType::Boolean,
          std::wstring_view configSourceName = L"",
          int configSourceLineNumber = 0)
      {
        return InsertInternal(
            section,
            name,
            Value(
                static_cast<TBooleanValue>(boolValue),
                valueType,
                *(configSourceNames.emplace(configSourceName).first),
                configSourceLineNumber));
      }

      /// Stores a new string value for the specified configuration setting in the specified
      /// section. Will fail if the value already exists.
      /// @param [in] section Section into which to insert the configuration setting.
      /// @param [in] name Name of the configuration setting into which to insert the value.
      /// @param [in] stringValue Value to insert.
      /// @param [in] valueType Type enumerator for the value to be parsed and possibly inserted.
      /// This is a separate parameter because it additionally allows single- or multi-valued to be
      /// specified. Defaults to the correct single-valued enumerator for this overload.
      /// @param [in] configSourceName Name of the configuration data source, such as a
      /// configuration file. Defaults to empty.
      /// @param [in] configSourceLineNumber Line number within the configuration data source of the
      /// value being inserted. Defaults to 0.
      /// @return `true` on success, `false` on failure.
      inline bool Insert(
          std::wstring_view section,
          std::wstring_view name,
          TStringValue&& stringValue,
          EValueType valueType = EValueType::String,
          std::wstring_view configSourceName = L"",
          int configSourceLineNumber = 0)
      {
        return InsertInternal(
            section,
            name,
            Value(
                std::move(stringValue),
                valueType,
                *(configSourceNames.emplace(configSourceName).first),
                configSourceLineNumber));
      }

      /// Determines if this configuration data object is empty (i.e. contains no
      /// configuration data).
      /// @return `true` if so, `false` otherwise.
      inline bool Empty(void) const
      {
        return sections.empty();
      }

      /// Allows read-only access to all sections.
      /// Useful for iterating.
      /// @return Container of all sections.
      inline const TSections& Sections(void) const
      {
        return sections;
      }

      /// Converts the entire contents of this object into a configuration file string.
      /// @return String that contains a configuration file representation of the data held by
      /// this object.
      std::wstring ToConfigurationFileString(void) const;

    private:

      /// Internal implementation of storing a new value into this object.
      /// @param [in] section Section into which to insert the configuration setting.
      /// @param [in] name Name of the configuration setting into which to insert the value.
      /// @param [in] value Value to insert.
      /// @return `true` on success, `false` on failure.
      bool InsertInternal(std::wstring_view section, std::wstring_view name, Value&& value);

      /// Holds the names of all configuration sources from which values in this object were
      /// obtained.
      TConfigSourceNames configSourceNames;

      /// Holds configuration data at the level of entire sections, one element per section.
      TSections sections;
    };

    /// Internal structure for holding the state of an overall configuration file read operation.
    struct SReadState;

    /// Interface for reading and parsing INI-formatted configuration files.
    /// Name-and-value pairs (of the format "name = value") are namespaced by sections (of the
    /// format "[section name]"). Provides basic configuration file reading and parsing
    /// functionality, but leaves managing and error-checking configuration values to
    /// subclasses.
    class ConfigurationFileReader
    {
    public:

      virtual ~ConfigurationFileReader(void) = default;

      /// Attempts to expands all macros present in the specified input string. Intended for
      /// internal use, but exposed for testing.
      /// @param [in] unexpandedString Unexpanded input string to be searched for macros.
      /// @param [in] currentConfigSourceName Name of the current configuration source, which is
      /// used to generate some of the macro expansions. Defaults to an empty string. If not
      /// supplied or invalid, macros that reference the current configuration source name cannot be
      /// expanded.
      /// @return Either the input string with all macros fully expanded or an error message
      /// explaining why the expansion failed.
      static ValueOrError<TemporaryString, TemporaryString> ExpandAllMacros(
          std::wstring_view unexpandedString,
          std::wstring_view currentConfigSourceName = std::wstring_view());

      /// Retrieves and returns the error messages that arose during the configuration file
      /// read attempt that produced this object. Does not check that error messages
      /// actually exist.
      /// @return Error messages from last configuration file read attempt.
      inline const std::vector<std::wstring>& GetErrorMessages(void) const
      {
        return *errorMessages;
      }

      /// Specifies whether or not any errors arose during the configuration file read attempt
      /// that produced this object. More details on any errors that arose are available by
      /// examining the error messages, unless they have already been cleared
      /// @return `true` if so, `false` if not.
      inline bool HasErrorMessages(void) const
      {
        return errorMessages.has_value();
      }

      /// Outputs all error messages to the log, if it is enabled for the specified severity.
      /// @param [in] indentNumSpaces Number of spaces to indent each generated log message.
      /// Defaults to 2.
      /// @param [in] severity Severity of each generated log message. Defaults to error.
      void LogAllErrorMessages(
          int indentNumSpaces = 2, Message ::ESeverity severity = Message::ESeverity::Error) const;

      /// Reads and parses a configuration file, storing the settings in the supplied
      /// configuration object. Intended to be invoked externally. Subclasses should not
      /// override this method.
      /// @param [in] configFileName Name of the configuration file to read. Defaults to the
      /// recommended configuration filename.
      /// @param [in] mustExist Indicates that it is an error for the configuration file not
      /// to exist. This requirement is unusual, so the default behavior is not to require it.
      /// @return Configuration data object filled based on the contents of the configuration
      /// file.
      ConfigurationData ReadConfigurationFile(
          std::wstring_view configFileName = RecommendedConfigurationFilePath(),
          bool mustExist = false);

      /// Reads and parses a configuration file held in memory, storing the settings in the
      /// supplied configuration object. Intended to be invoked externally, primarily by
      /// tests. Subclasses should not override this method.
      /// @param [in] configFileName Name of the configuration file to read.
      /// @return Configuration data object filled based on the contents of the configuration
      /// file.
      ConfigurationData ReadInMemoryConfigurationFile(std::wstring_view configBuffer);

    protected:

      /// Invoked at the beginning of a configuration file read operation.
      /// Subclasses are given the opportunity to initialize or reset any stored state, as
      /// needed. Overriding this method is optional, as a default implementation exists that
      /// does nothing.
      virtual void BeginRead(void);

      /// Invoked at the end of a configuration file read operation.
      /// Subclasses are given the opportunity to initialize or reset any stored state, as
      /// needed. Overriding this method is optional, as a default implementation exists that
      /// does nothing.
      virtual void EndRead(void);

      /// Specifies the action to take when a given section is encountered in a configuration
      /// file (i.e. the names that typically appear in [square brackets] and separate the
      /// configuration file into namespaces). Invoked while reading from a configuration
      /// file. Subclasses must override this method. They are allowed to process the section
      /// name however they see fit and indicate to the caller what action to take.
      /// @param [in] section Name of the section, as read from the configuration file.
      /// @return Action to take with the section.
      virtual Action ActionForSection(std::wstring_view section) = 0;

      /// Invoked to allow the subclass to process the specified integer-typed configuration
      /// setting, identified by enclosing section name and by configuration setting name.
      /// Subclasses are allowed to process the value however they see fit and indicate to the
      /// caller what action to take. Any values passed as read-only views are backed by
      /// temporary memory that will be discarded upon method return. Subclasses should copy
      /// values that need to be preserved outside of the configuration data structure.
      /// @param [in] section Name of the enclosing section, as read from the configuration
      /// file.
      /// @param [in] name Name of the configuration setting, as read from the configuration
      /// file.
      /// @param [in] value View of the value of the configuration setting, as read and parsed
      /// from the configuration file.
      /// @return Action to take with the name/value pair.
      virtual Action ActionForValue(
          std::wstring_view section, std::wstring_view name, TIntegerView value) = 0;

      /// Invoked to allow the subclass to process the specified Boolean-typed configuration
      /// setting, identified by enclosing section name and by configuration setting name.
      /// Subclasses are allowed to process the value however they see fit and indicate to the
      /// caller what action to take. Any values passed as read-only views are backed by
      /// temporary memory that will be discarded upon method return. Subclasses should copy
      /// values that need to be preserved outside of the configuration data structure.
      /// @param [in] section Name of the enclosing section, as read from the configuration
      /// file.
      /// @param [in] name Name of the configuration setting, as read from the configuration
      /// file.
      /// @param [in] value View of the value of the configuration setting, as read and parsed
      /// from the configuration file.
      /// @return Action to take with the name/value pair.
      virtual Action ActionForValue(
          std::wstring_view section, std::wstring_view name, TBooleanView value) = 0;

      /// Invoked to allow the subclass to process specified string-typed configuration
      /// setting, identified by enclosing section name and by configuration setting name.
      /// Subclasses are allowed to process the value however they see fit and indicate to the
      /// caller what action to take. Any values passed as read-only views are backed by
      /// temporary memory that will be discarded upon method return. Subclasses should copy
      /// values that need to be preserved outside of the configuration data structure.
      /// @param [in] section Name of the enclosing section, as read from the configuration
      /// file.
      /// @param [in] name Name of the configuration setting, as read from the configuration
      /// file.
      /// @param [in] value View of the value of the configuration setting, as read and parsed
      /// from the configuration file.
      /// @return Action to take with the name/value pair.
      virtual Action ActionForValue(
          std::wstring_view section, std::wstring_view name, TStringView value) = 0;

      /// Specifies the type of the value for the given configuration setting.
      /// In lines that are of the form "name = value" parameters identify both the enclosing
      /// section and the name part. Subclasses should override this method. For example, if
      /// the value is expected to be an integer, subclasses should indicate this so the value
      /// is parsed and submitted correctly.
      /// @param [in] section Name of the enclosing section, as read from the configuration
      /// file.
      /// @param [in] name Name of the configuration setting (the left part of the example
      /// line given above).
      /// @return Type to associate with the value (the right part of the example line given
      /// above), which can be an error if the particular configuration setting is not
      /// supported.
      virtual EValueType TypeForValue(std::wstring_view section, std::wstring_view name) = 0;

    private:

      /// Appends an error message into the list of error messages associated with the file read
      /// attempt. Each such error message is a semantically-rich description of an error that
      /// occurred during the configuration file read attempt that fills this object.
      /// @param [in] readState Structure that keeps track of the state of the overall read
      /// operation.
      /// @param [in] errorMessage Error message to append.
      void AppendErrorMessage(const SReadState& readState, std::wstring_view errorMessage);

      /// Implements an "include" directive in a configuration file.
      /// @param [in,out] readState Structure that keeps track of the state of the overall read
      /// operation.
      /// @param [in] directiveString Directive read from the configuration file. Used exclusively
      /// for error message generation, if needed.
      /// @param [in] maybeParamsString Parameter string for the "include" directive, if provided in
      /// the configuration file.
      /// @param [in] includeIsRequired Specifies whether or not the inclusion of the other
      /// configuration file must succeed to avoid an error. If `false` then the "include" directive
      /// is considered optional and it is not an error if the file does not exist.
      void HandleIncludeDirective(
          SReadState& readState,
          std::wstring_view directiveString,
          std::optional<std::wstring_view> maybeUnparsedDirectiveParamString,
          bool includeIsRequired);

      /// Internal implementation of parsing and possibly acting on a directive represented in a
      /// configuration file line. Directives begin with the '%' character and modify how
      /// configuration files are processed, such as by including another file, skipping over lines,
      /// and so on.
      /// @param [in,out] readState Structure that keeps track of the state of the overall read
      /// operation.
      /// @param [in] configLineTrimmed Unparsed configuration line, as read from the configuration
      /// file, with whitespace trimmed.
      void ParseAndMaybeHandleDirective(SReadState& readState, std::wstring_view configLineTrimmed);

      /// Internal implementation of parsing, querying subclasses for the desired action, and
      /// possibly inserting a section read from a configuration file into a configuration data
      /// object.
      /// @param [in,out] readState Structure that keeps track of the state of the overall read
      /// operation.
      /// @param [in] configLineTrimmed Unparsed configuration line, as read from the configuration
      /// file, with whitespace trimmed.
      void ParseAndMaybeInsertSection(SReadState& readState, std::wstring_view configLineTrimmed);

      /// Internal implementation of parsing, querying subclasses for the desired action, and
      /// possibly inserting a value read from a configuration file into a configuration data
      /// object.
      /// @tparam ValueType Type of value to be parsed and possibly inserted.
      /// @param [in,out] readState Structure that keeps track of the state of the overall read
      /// operation.
      /// @param [in] name Name of the configuration setting.
      /// @param [in] valueType Type enumerator for the value to be parsed and possibly inserted.
      /// This is a separate parameter from the template parameter because it additionally allows
      /// single- or multi-valued to be specified.
      /// @param [in] valueUnparsed Unparsed value read directly from the configuration file.
      template <typename ValueType> void ParseAndMaybeInsertValue(
          SReadState& readState,
          std::wstring_view name,
          EValueType valueType,
          std::wstring_view valueUnparsed);

      /// Internal implementation of reading and parsing configuration files from any source.
      /// @param [in,out] readState Structure that keeps track of the state of the overall read
      /// operation.
      void ReadConfigurationInternal(SReadState& readState);

      /// Holds the error messages that describes any errors that occurred during
      /// configuration file read.
      std::optional<std::vector<std::wstring>> errorMessages;
    };

    /// Type alias for a suggested format for storing the supported layout of a section within a
    /// configuration file. Useful for pre-determining what is allowed to appear within one
    /// section of a configuration file. Names of configuration settings are case-insensitive.
    using TConfigurationFileSectionLayout = std::
        map<std::wstring_view, EValueType, Strings::CaseInsensitiveLessThanComparator<wchar_t>>;

    /// Type alias for a suggested format for storing the supported layout of a configuration
    /// file. Useful for pre-determining what is allowed to appear within a configuration file.
    /// Names of sections are case-insensitive.
    using TConfigurationFileLayout = std::map<
        std::wstring_view,
        TConfigurationFileSectionLayout,
        Strings::CaseInsensitiveLessThanComparator<wchar_t>>;
  } // namespace Configuration
} // namespace Infra
