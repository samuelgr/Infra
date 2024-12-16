/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2024
 ***********************************************************************************************//**
 * @file ConfigurationTest.cpp
 *   Unit tests for configuration file functionality.
 **************************************************************************************************/

#include "Core/Configuration.h"

#include <functional>
#include <optional>

#include "Core/TemporaryBuffer.h"
#include "Test/TestCase.h"

namespace CoreInfraTest
{
  using namespace ::Infra::Configuration;
  using ::Infra::TemporaryString;

  /// Configuration file reader concrete implementation useful for tests. In general, actions and
  /// types are parsed out of the names of sections and configuration values, which is intended to
  /// make writing tests easy without the overhead of also creating a custom reader type each time.
  class TestConfigurationFileReader : public ConfigurationFileReader
  {
  public:

    /// Type alias for a function that overrides the mapping from section/name pair to value type.
    /// If it returns nothing, then the override does not occur. Otherwise it modifies the mapping
    /// from the default implemented in this class.
    using TOverrideTypeForValueFunc =
        std::function<std::optional<EValueType>(std::wstring_view section, std::wstring_view name)>;

    TestConfigurationFileReader(
        TOverrideTypeForValueFunc overrideTypeForValueFunc =
            [](std::wstring_view, std::wstring_view) -> std::optional<EValueType>
        {
          return std::nullopt;
        })
        : overrideTypeForValueFunc(overrideTypeForValueFunc)
    {}

    Action ActionForString(std::wstring_view str)
    {
      if (str.contains(L"Error")) return Action::Error();
      if (str.contains(L"Skip")) return Action::Skip();
      return Action::Process();
    }

  protected:

    // ConfigurationFileReader
    Action ActionForSection(std::wstring_view section) override
    {
      if (kSectionNameGlobal == section) return Action::Process();
      return ActionForString(section);
    }

    Action ActionForValue(
        std::wstring_view section, std::wstring_view name, TIntegerView value) override
    {
      return ActionForString(name);
    }

    Action ActionForValue(
        std::wstring_view section, std::wstring_view name, TBooleanView value) override
    {
      return ActionForString(name);
    }

    Action ActionForValue(
        std::wstring_view section, std::wstring_view name, TStringView value) override
    {
      return ActionForString(name);
    }

    EValueType TypeForValue(std::wstring_view section, std::wstring_view name) override
    {
      std::optional<EValueType> maybeOverrideMapping = overrideTypeForValueFunc(section, name);
      if (maybeOverrideMapping.has_value()) return *maybeOverrideMapping;

      if (name.contains(L"IntegerMulti")) return EValueType::IntegerMultiValue;
      if (name.contains(L"BooleanMulti")) return EValueType::BooleanMultiValue;
      if (name.contains(L"StringMulti")) return EValueType::StringMultiValue;
      if (name.contains(L"Integer")) return EValueType::Integer;
      if (name.contains(L"Boolean")) return EValueType::Boolean;
      if (name.contains(L"String")) return EValueType::String;
      return EValueType::Error;
    }

  private:

    TOverrideTypeForValueFunc overrideTypeForValueFunc;
  };

  // Verifies that integer values can be created and retrieved correctly. Checks that the resulting
  // object correctly identifies its own type and holds the correct value.
  TEST_CASE(Configuration_Value_CreateAndGetInteger)
  {
    constexpr int kTestInt = 33;

    for (EValueType valueType : {EValueType::Integer, EValueType::IntegerMultiValue})
    {
      const Value testIntValue(kTestInt, valueType);
      TEST_ASSERT(valueType == testIntValue.GetType());
      TEST_ASSERT(true == testIntValue.TypeIsInteger());
      TEST_ASSERT(false == testIntValue.TypeIsBoolean());
      TEST_ASSERT(false == testIntValue.TypeIsString());
      TEST_ASSERT(kTestInt == testIntValue);
    }
  }

  // Verifies that Boolean values can be created and retrieved correctly. Checks that the resulting
  // object correctly identifies its own type and holds the correct value.
  TEST_CASE(Configuration_Value_CreateAndGetBoolean)
  {
    constexpr bool kTestBool = true;

    for (EValueType valueType : {EValueType::Boolean, EValueType::BooleanMultiValue})
    {
      const Value testBoolValue(true, valueType);
      TEST_ASSERT(valueType == testBoolValue.GetType());
      TEST_ASSERT(false == testBoolValue.TypeIsInteger());
      TEST_ASSERT(true == testBoolValue.TypeIsBoolean());
      TEST_ASSERT(false == testBoolValue.TypeIsString());
      TEST_ASSERT(kTestBool == testBoolValue);
    }
  }

  // Verifies that string values can be created and retrieved correctly. Checks that the resulting
  // object correctly identifies its own type and holds the correct value.
  TEST_CASE(Configuration_Value_CreateAndGetString)
  {
    constexpr std::wstring_view kTestString = L"Test string.";

    for (EValueType valueType : {EValueType::String, EValueType::StringMultiValue})
    {
      const Value testStringValue(kTestString, valueType);
      TEST_ASSERT(valueType == testStringValue.GetType());
      TEST_ASSERT(false == testStringValue.TypeIsInteger());
      TEST_ASSERT(false == testStringValue.TypeIsBoolean());
      TEST_ASSERT(true == testStringValue.TypeIsString());
      TEST_ASSERT(kTestString == testStringValue);
    }
  }

  // Verifies that equalty and less-than comparison of integers operate correctly even when a mix of
  // single-valued and multi-valued types are present.
  TEST_CASE(Configuration_Value_CompareIntegers)
  {
    const Value testValueA(1, EValueType::Integer);
    const Value testValueB(1, EValueType::IntegerMultiValue);
    const Value testValueC(0, EValueType::Integer);
    const Value testValueD(0, EValueType::IntegerMultiValue);

    TEST_ASSERT(testValueA == Value(testValueA));
    TEST_ASSERT(testValueA == testValueB);
    TEST_ASSERT(testValueA != testValueC);
    TEST_ASSERT(testValueA != testValueD);

    TEST_ASSERT(testValueC < testValueA);
    TEST_ASSERT(testValueD < testValueA);
    TEST_ASSERT(testValueC < testValueB);
    TEST_ASSERT(testValueD < testValueB);
  }

  // Verifies that equalty and less-than comparison of Booleans operate correctly even when a mix of
  // single-valued and multi-valued types are present.
  TEST_CASE(Configuration_Value_CompareBooleans)
  {
    const Value testValueA(true, EValueType::Boolean);
    const Value testValueB(true, EValueType::BooleanMultiValue);
    const Value testValueC(false, EValueType::Boolean);
    const Value testValueD(false, EValueType::BooleanMultiValue);

    TEST_ASSERT(testValueA == Value(testValueA));
    TEST_ASSERT(testValueA == testValueB);
    TEST_ASSERT(testValueA != testValueC);
    TEST_ASSERT(testValueA != testValueD);

    TEST_ASSERT(testValueC < testValueA);
    TEST_ASSERT(testValueD < testValueA);
    TEST_ASSERT(testValueC < testValueB);
    TEST_ASSERT(testValueD < testValueB);
  }

  // Verifies that equalty and less-than comparison of strings operate correctly even when a mix of
  // single-valued and multi-valued types are present.
  TEST_CASE(Configuration_Value_CompareStrings)
  {
    const Value testValueA(L"BBBB", EValueType::String);
    const Value testValueB(L"BBBB", EValueType::StringMultiValue);
    const Value testValueC(L"AAAA", EValueType::String);
    const Value testValueD(L"AAAA", EValueType::StringMultiValue);

    TEST_ASSERT(testValueA == Value(testValueA));
    TEST_ASSERT(testValueA == testValueB);
    TEST_ASSERT(testValueA != testValueC);
    TEST_ASSERT(testValueA != testValueD);

    TEST_ASSERT(testValueC < testValueA);
    TEST_ASSERT(testValueD < testValueA);
    TEST_ASSERT(testValueC < testValueB);
    TEST_ASSERT(testValueD < testValueB);
  }

  // Verifies that values are not equal to one another if the types are different.
  TEST_CASE(Configuration_Value_CompareDifferentTypes)
  {
    const Value testValueInteger(0);
    const Value testValueBoolean(false);
    const Value testValueString(L"\0");

    TEST_ASSERT(testValueInteger != testValueBoolean);
    TEST_ASSERT(testValueBoolean != testValueString);
    TEST_ASSERT(testValueString != testValueInteger);
  }

  // Verifies that integer values can be extracted and that the resulting configuration value is
  // considered empty and typeless.
  TEST_CASE(Configuration_Value_ExtractInteger)
  {
    Value testIntValue(55);
    const auto expectedValue = testIntValue.GetInteger();
    const auto actualValue = testIntValue.ExtractInteger();
    TEST_ASSERT(actualValue == expectedValue);
    TEST_ASSERT(false == testIntValue.TypeIsInteger());
    TEST_ASSERT(false == testIntValue.TypeIsBoolean());
    TEST_ASSERT(false == testIntValue.TypeIsString());
  }

  // Verifies that Boolean values can be extracted and that the resulting configuration value is
  // considered empty and typeless.
  TEST_CASE(Configuration_Value_ExtractBoolean)
  {
    Value testBoolValue(true);
    const bool expectedValue = testBoolValue;
    const bool actualValue = testBoolValue.Extract<TBooleanValue>();
    TEST_ASSERT(actualValue == expectedValue);
    TEST_ASSERT(false == testBoolValue.TypeIsInteger());
    TEST_ASSERT(false == testBoolValue.TypeIsBoolean());
    TEST_ASSERT(false == testBoolValue.TypeIsString());
  }

  // Verifies that string values can be extracted using move semantics and that the resulting
  // configuration value is considered empty and typeless.
  TEST_CASE(Configuration_Value_ExtractString)
  {
    // This string must be long enough to avoid small-string optimizations, otherwise the buffer
    // pointer equality check is invalid even if move semantics are correctly used.
    Value testStringValue(L"Some unimportant string goes here.");
    const auto expectedBufferPointer = testStringValue.GetString().data();
    const auto extractedStringValue = testStringValue.ExtractString();
    const auto actualBufferPointer = extractedStringValue.data();
    TEST_ASSERT(actualBufferPointer == expectedBufferPointer);
    TEST_ASSERT(false == testStringValue.TypeIsInteger());
    TEST_ASSERT(false == testStringValue.TypeIsBoolean());
    TEST_ASSERT(false == testStringValue.TypeIsString());
  }

  // Verifies that configuration files correctly store and can retrieve metadata including
  // configuration file name and line number.
  TEST_CASE(Configuration_Value_GetMetadata)
  {
    constexpr int kTestInteger = 100;
    constexpr std::wstring_view kTestConfigFileName = L"C:\\Test\\Config.ini";
    constexpr int kTestLineNumber = 1000;

    const Value testConfigValue(
        kTestInteger, EValueType::Integer, kTestConfigFileName, kTestLineNumber);
    TEST_ASSERT(kTestConfigFileName == testConfigValue.GetConfigFileName());
    TEST_ASSERT(kTestLineNumber == testConfigValue.GetConfigFileLineNumber());
  }

  // Verifies that comparison works correctly between Value objects and underlying values of the
  // possible held types.
  TEST_CASE(Configuration_Value_EqualityCompareWithLiterals)
  {
    const Value testConfigValueInteger(55);
    TEST_ASSERT(testConfigValueInteger == 55);
    TEST_ASSERT(testConfigValueInteger != 1234);
    TEST_ASSERT(testConfigValueInteger != true);
    TEST_ASSERT(testConfigValueInteger != L"Some string.");

    const Value testConfigValueBoolean(true);
    TEST_ASSERT(testConfigValueBoolean == true);
    TEST_ASSERT(testConfigValueBoolean != 1234);
    TEST_ASSERT(testConfigValueBoolean != false);
    TEST_ASSERT(testConfigValueInteger != L"Some string.");

    const Value testConfigValueString(L"Test string 123");
    TEST_ASSERT(testConfigValueString == L"Test string 123");
    TEST_ASSERT(testConfigValueBoolean != 1234);
    TEST_ASSERT(testConfigValueBoolean != false);
    TEST_ASSERT(testConfigValueString != L"Another string 456");
  }

  // Verifies that metadata is correctly retrieved for empty Name objects.
  TEST_CASE(Configuration_Name_CreateEmptyAndQueryMetadata)
  {
    const Name testConfigName;
    TEST_ASSERT(true == testConfigName.Empty());
    TEST_ASSERT(0 == testConfigName.Count());

    TEST_ASSERT(EValueType::Integer != testConfigName.GetType());
    TEST_ASSERT(EValueType::Boolean != testConfigName.GetType());
    TEST_ASSERT(EValueType::String != testConfigName.GetType());

    TEST_ASSERT(false == testConfigName.TypeIsInteger());
    TEST_ASSERT(false == testConfigName.TypeIsBoolean());
    TEST_ASSERT(false == testConfigName.TypeIsString());
  }

  // Verifies that metadata is correctly retrieved for Name objects with values.
  TEST_CASE(Configuration_Name_CreateWithValuesAndQueryMetadata)
  {
    const Name testConfigName({11, 22, 33, 44, 55});
    TEST_ASSERT(false == testConfigName.Empty());
    TEST_ASSERT(5 == testConfigName.Count());
    TEST_ASSERT(EValueType::IntegerMultiValue == testConfigName.GetType());
    TEST_ASSERT(true == testConfigName.TypeIsInteger());
  }

  // Verifies that insertion occurs correctly for values all of the same type into a configuration
  // setting object.
  TEST_CASE(Configuration_Name_Insert_SuccessValuesSameType)
  {
    Name testConfigName;
    for (int i = 0; i < 5; ++i)
      TEST_ASSERT(true == testConfigName.Insert(i));

    int i = 0;
    auto valueIter = testConfigName.Values().cbegin();
    while (valueIter != testConfigName.Values().cend())
    {
      TEST_ASSERT(i == *valueIter);
      valueIter++;
      i += 1;
    }
  }

  // Verifies that insertion fails when there is a type mismatch between existing values and a new
  // value being inserted into a configuration setting object.
  TEST_CASE(Configuration_Name_Insert_FailureValuesDifferentType)
  {
    Name testConfigName(0);
    TEST_ASSERT(true == testConfigName.Insert(1));
    TEST_ASSERT(false == testConfigName.Insert(L"string"));
    TEST_ASSERT(false == testConfigName.Insert(true));
    TEST_ASSERT(2 == testConfigName.Count());
  }

  // Verifies that extraction of the first contained value occurs correctly and uses move semantics.
  TEST_CASE(Configuration_Name_ExtractFirst)
  {
    // This string must be long enough to avoid small-string optimizations, otherwise the buffer
    // pointer equality check is invalid even if move semantics are correctly used.
    Name testConfigName(L"Some unimportant string goes here.");
    const auto expectedBufferPointer = testConfigName.GetFirst().GetString().data();
    const auto extractedStringValue = testConfigName.ExtractFirstString();
    TEST_ASSERT(true == extractedStringValue.has_value());

    const auto actualBufferPointer = extractedStringValue->data();
    TEST_ASSERT(actualBufferPointer == expectedBufferPointer);
  }

  // Verifies that extraction of all contained values occurs correctly and uses move semantics.
  TEST_CASE(Configuration_Name_ExtractAll)
  {
    // These strings must be long enough to avoid small-string optimizations, otherwise the buffer
    // pointer equality check is invalid even if move semantics are correctly used.
    Name testConfigName(
        {L"String 111111", L"String 222222", L"String 333333", L"String 444444", L"String 555555"});

    std::vector<const wchar_t*> expectedBufferPointers;
    for (const auto& value : testConfigName.Values())
      expectedBufferPointers.push_back(value.GetString().data());

    const auto extractedStringValues = testConfigName.ExtractAllStrings();
    TEST_ASSERT(true == extractedStringValues.has_value());

    std::vector<const wchar_t*> actualBufferPointers;
    for (const auto& extractedString : *extractedStringValues)
      actualBufferPointers.push_back(extractedString.data());
    TEST_ASSERT(actualBufferPointers == expectedBufferPointers);
  }

  // Verifies that obtaining the first value occurs correctly both by method call and by operator
  // overload.
  TEST_CASE(Configuration_Name_GetFirst)
  {
    const Name testConfigName({1, 2, 3, 4, 5});
    TEST_ASSERT(1 == testConfigName.GetFirst());
    TEST_ASSERT(1 == testConfigName->GetInteger());
    TEST_ASSERT(1 == (*testConfigName));
  }

  // Verifies that actual and default values are correctly retrieved when attempting to retrieve the
  // first value of a configuration setting and specifying an alternative default value.
  TEST_CASE(Configuration_Name_ValueOr)
  {
    const Name testConfigNameInteger({1, 2, 3, 4, 5});
    TEST_ASSERT(1 == testConfigNameInteger.ValueOr(0));
    TEST_ASSERT(false == testConfigNameInteger.ValueOr(false));
    TEST_ASSERT(L"string" == testConfigNameInteger.ValueOr(L"string"));

    const Name testConfigNameBoolean({false});
    TEST_ASSERT(0 == testConfigNameBoolean.ValueOr(0));
    TEST_ASSERT(false == testConfigNameBoolean.ValueOr(true));
    TEST_ASSERT(L"string" == testConfigNameBoolean.ValueOr(L"string"));

    const Name testConfigNameString({L"test string"});
    TEST_ASSERT(0 == testConfigNameString.ValueOr(0));
    TEST_ASSERT(false == testConfigNameString.ValueOr(false));
    TEST_ASSERT(L"test string" == testConfigNameString.ValueOr(L"string"));

    const Name testConfigNameEmpty;
    TEST_ASSERT(55 == testConfigNameEmpty.ValueOr(55));
    TEST_ASSERT(true == testConfigNameEmpty.ValueOr(true));
    TEST_ASSERT(L"string" == testConfigNameEmpty.ValueOr(L"string"));
  }

  // Verifies that metadata is correctly retrieved for empty Section objects.
  TEST_CASE(Configuration_Section_CreateEmptyAndQueryMetadata)
  {
    const Section testConfigSection;
    TEST_ASSERT(0 == testConfigSection.Count());
    TEST_ASSERT(true == testConfigSection.Empty());
  }

  // Verifies that metadata is correctly retrieved for Section objects with values.
  TEST_CASE(Configuration_Section_CreateWithValuesAndQueryMetadata)
  {
    const Section testConfigSection({{L"Setting1", {1, 2, 3, 4, 5}}, {L"Setting2", {true}}});
    TEST_ASSERT(2 == testConfigSection.Count());
    TEST_ASSERT(false == testConfigSection.Empty());
    TEST_ASSERT(true == testConfigSection.Contains(L"Setting1"));
    TEST_ASSERT(true == testConfigSection.Contains(L"Setting2"));
  }

  // Verifies that insertion occurs correctly for values all of the same type into a configuration
  // section object.
  TEST_CASE(Configuration_Section_Insert_SuccessValuesSameType)
  {
    Section testConfigSection;
    for (int i = 0; i < 5; ++i)
      TEST_ASSERT(true == testConfigSection.Insert(L"TestSetting", i));

    TEST_ASSERT(true == testConfigSection.Contains(L"TestSetting"));

    int i = 0;
    auto valueIter = testConfigSection[L"TestSetting"].Values().cbegin();
    while (valueIter != testConfigSection[L"TestSetting"].Values().cend())
    {
      TEST_ASSERT(i == *valueIter);
      valueIter++;
      i += 1;
    }
  }

  // Verifies that insertion fails when there is a type mismatch between existing values and a new
  // value being inserted into a configuration section object.
  TEST_CASE(Configuration_Section_FailureValuesDifferentType)
  {
    Section testConfigSection({{L"TestSetting", 0}});
    TEST_ASSERT(true == testConfigSection.Insert(L"TestSetting", 1));
    TEST_ASSERT(false == testConfigSection.Insert(L"TestSetting", L"string"));
    TEST_ASSERT(false == testConfigSection.Insert(L"TestSetting", true));
    TEST_ASSERT(2 == testConfigSection[L"TestSetting"].Count());
  }

  // Verifies that extraction of the first contained setting occurs correctly and uses move
  // semantics.
  TEST_CASE(Configuration_Section_ExtractFirst)
  {
    Section testConfigSection({{L"TestSetting", L"Nice long test configuration setting string."}});
    const auto expectedBufferPointer = testConfigSection[L"TestSetting"]->GetString().data();

    const auto extractedConfigName = testConfigSection.ExtractFirst();
    TEST_ASSERT(true == extractedConfigName.has_value());
    TEST_ASSERT(L"TestSetting" == extractedConfigName->first);

    const auto actualBufferPointer = extractedConfigName->second->GetString().data();
    TEST_ASSERT(actualBufferPointer == expectedBufferPointer);
  }

  // Verifies that obtaining the first setting occurs correctly.
  TEST_CASE(Configuration_Section_GetFirst)
  {
    const Name expectedConfigName({1, 2, 3, 4, 5});

    const Section testConfigSection({{L"TestSetting", expectedConfigName}});
    const auto actualConfigNamePair = testConfigSection.GetFirst();
    TEST_ASSERT(L"TestSetting" == actualConfigNamePair->first);

    const Name& actualConfigName = actualConfigNamePair->second;
    TEST_ASSERT(actualConfigName == expectedConfigName);
  }

  // Verifies that indexing a section using a setting name that does not exist correctly results in
  // an empty default Name object.
  TEST_CASE(Configuration_Section_IndexNonExistentName)
  {
    const Section testConfigSection;
    const Name& expectedEmptyConfigName = testConfigSection[L"Name that doesn't exist"];
    TEST_ASSERT(true == expectedEmptyConfigName.Empty());
  }

  // Verifies that metadata is correctly retrieved for empty ConfigurationData objects.
  TEST_CASE(Configuration_ConfigurationData_CreateEmptyAndQueryMetadata)
  {
    const ConfigurationData testConfigData;
    TEST_ASSERT(0 == testConfigData.Count());
    TEST_ASSERT(true == testConfigData.Empty());
  }

  // Verifies that metadata is correctly retrieved for ConfigurationData objects with values.
  TEST_CASE(Configuration_ConfigurationData_CreateWithValuesAndQueryMetadata)
  {
    const ConfigurationData testConfigData(
        {{L"Section1", Section({{L"Setting1", {1, 2, 3, 4, 5}}, {L"Setting2", {true}}})},
         {L"Section2",
          Section({{L"Setting3", L"Value for setting3"}, {L"Setting4", {5, 6, 7, 8}}})}});

    TEST_ASSERT(2 == testConfigData.Count());
    TEST_ASSERT(false == testConfigData.Empty());

    TEST_ASSERT(true == testConfigData.Contains(L"Section1"));
    TEST_ASSERT(true == testConfigData.Contains(L"Section1", L"Setting1"));
    TEST_ASSERT(true == testConfigData.Contains(L"Section1", L"Setting2"));

    TEST_ASSERT(true == testConfigData.Contains(L"Section2"));
    TEST_ASSERT(true == testConfigData.Contains(L"Section2", L"Setting3"));
    TEST_ASSERT(true == testConfigData.Contains(L"Section2", L"Setting4"));
  }

  // Verifies that values can be inserted correctly into a ConfigurationData object.
  TEST_CASE(Configuration_ConfigurationData_Insert)
  {
    ConfigurationData testConfigData;
    for (int i = 0; i < 5; ++i)
      TEST_ASSERT(true == testConfigData.Insert(L"TestSection", L"TestName", i));

    TEST_ASSERT(5 == testConfigData[L"TestSection"][L"TestName"].Count());

    int i = 0;
    auto valueIter = testConfigData[L"TestSection"][L"TestName"].Values().cbegin();
    while (valueIter != testConfigData[L"TestSection"][L"TestName"].Values().cbegin())
    {
      TEST_ASSERT(i == *valueIter);
      valueIter++;
      i += 1;
    }
  }

  // Verifies that a configuration value can be accessed using subscript indexing operators when
  // both the section and the configuration name exist.
  TEST_CASE(Configuration_ConfigurationData_GetUsingIndexingExists)
  {
    const ConfigurationData testConfigData(
        {{L"Section1", Section({{L"Setting1", {1, 2, 3, 4, 5}}, {L"Setting2", {true}}})},
         {L"Section2",
          Section({{L"Setting3", L"Value for setting3"}, {L"Setting4", {5, 6, 7, 8}}})}});
    TEST_ASSERT(true == testConfigData[L"Section2"][L"Setting4"].HasValue());
    TEST_ASSERT(5 == *testConfigData[L"Section2"][L"Setting4"]);
    TEST_ASSERT(5 == testConfigData[L"Section2"][L"Setting4"].ValueOr(0));
  }

  // Verifies that a configuration value can be accessed using subscript indexing operators when
  // either the section or the configuration name do not exist. In this case the resulting object is
  // empty.
  TEST_CASE(Configuration_ConfigurationData_GetUsingIndexingDoesNotExist)
  {
    const ConfigurationData testConfigData(
        {{L"Section1", Section({{L"Setting1", {1, 2, 3, 4, 5}}, {L"Setting2", {true}}})},
         {L"Section2",
          Section({{L"Setting3", L"Value for setting3"}, {L"Setting4", {5, 6, 7, 8}}})}});
    TEST_ASSERT(false == testConfigData[L"Section2"][L"BadSetting"].HasValue());
    TEST_ASSERT(false == testConfigData[L"BadSection"][L"OtherBadSetting"].HasValue());
  }

  // Verifies that a configuration file string is correctly generated from an initialized
  // ConfigurationData object.
  TEST_CASE(Configuration_ConfigurationData_ToConfigFileString)
  {
    const ConfigurationData testConfigData(
        {{L"Section1", Section({{L"Setting1", {1, 2, 3, 4, 5}}, {L"Setting2", {true}}})},
         {L"Section2",
          Section({{L"Setting3", L"Value for setting3"}, {L"Setting4", {5, 6, 7, 8}}})}});
    constexpr std::wstring_view expectedConfigFileString =
        L"[Section1]\n"
        L"Setting1 = 1\n"
        L"Setting1 = 2\n"
        L"Setting1 = 3\n"
        L"Setting1 = 4\n"
        L"Setting1 = 5\n"
        L"Setting2 = yes\n"
        L"[Section2]\n"
        L"Setting3 = Value for setting3\n"
        L"Setting4 = 5\n"
        L"Setting4 = 6\n"
        L"Setting4 = 7\n"
        L"Setting4 = 8\n";
    const auto actualConfigFileString = testConfigData.ToConfigurationFileString();
    TEST_ASSERT(actualConfigFileString == expectedConfigFileString);
  }

  // Verifies that a valid configuration file is successfully read and all lines are properly
  // processed into a configuration data object.
  TEST_CASE(Configuration_ConfigurationFileReader_ReadSuccessAllProcessed)
  {
    constexpr std::wstring_view kTestConfigFile =
        L"   GlobalBooleanSetting    =          yes    \n"
        L"AnotherGlobalBooleanSetting=no  \n"
        L"\n"
        L"; This is a comment\n"
        L"   ### This is another comment\n"
        L"   [Section1]   \n"
        L"IntegerSetting = 66\n"
        L"StringSetting =    string value  with    spaces that should be preserved except at the ends     \n"
        L"  [ Section2 ]  \n"
        L"IntegerMultiSetting = 1\n"
        L"IntegerMultiSetting = 2\n"
        L"IntegerMultiSetting = 3\n"
        L"IntegerMultiSetting = 4\n"
        L"IntegerMultiSetting = 5\n"
        L"     \n"
        L"     \n"
        L"\n";
    TestConfigurationFileReader testConfigReader;

    const ConfigurationData expectedConfigData(
        {{std::wstring(kSectionNameGlobal),
          Section({{L"GlobalBooleanSetting", true}, {L"AnotherGlobalBooleanSetting", false}})},
         {L"Section1",
          Section(
              {{L"IntegerSetting", 66},
               {L"StringSetting",
                L"string value  with    spaces that should be preserved except at the ends"}})},
         {L" Section2 ", Section({{L"IntegerMultiSetting", {1, 2, 3, 4, 5}}})}});
    const ConfigurationData actualConfigData =
        testConfigReader.ReadInMemoryConfigurationFile(kTestConfigFile);
    TEST_ASSERT(actualConfigData == expectedConfigData);
    TEST_ASSERT(false == testConfigReader.HasErrorMessages());
  }

  // Verifies that a valid configuration file is successfully read and, where requested, skipped
  // lines and sections are not processed into the configuration data object.
  TEST_CASE(Configuration_ConfigurationFileReader_ReadSuccessSomeSkipped)
  {
    constexpr std::wstring_view kTestConfigFile =
        L"   GlobalBooleanSetting    =          yes    \n"
        L"AnotherGlobalBooleanSettingSkip=no  \n"
        L"\n"
        L"; This is a comment\n"
        L"   ### This is another comment\n"
        L"   [Section1]   \n"
        L"IntegerSetting = 66\n"
        L"StringSetting =    string value  with    spaces that should be preserved except at the ends     \n"
        L"  [ Section2Skip ]  \n"
        L"IntegerMultiSetting = 1\n"
        L"IntegerMultiSetting = 2\n"
        L"IntegerMultiSetting = 3\n"
        L"IntegerMultiSetting = 4\n"
        L"IntegerMultiSetting = 5\n"
        L"     \n"
        L"     \n"
        L"\n";
    TestConfigurationFileReader testConfigReader;

    const ConfigurationData expectedConfigData(
        {{std::wstring(kSectionNameGlobal), Section({{L"GlobalBooleanSetting", true}})},
         {L"Section1",
          Section(
              {{L"IntegerSetting", 66},
               {L"StringSetting",
                L"string value  with    spaces that should be preserved except at the ends"}})}});
    const ConfigurationData actualConfigData =
        testConfigReader.ReadInMemoryConfigurationFile(kTestConfigFile);
    TEST_ASSERT(actualConfigData == expectedConfigData);
    TEST_ASSERT(false == testConfigReader.HasErrorMessages());
  }

  // Verifies that type parsing errors are correctly flagged. Does not check specific error
  // messages, just total count.
  TEST_CASE(Configuration_ConfigurationFileReader_ReadErrorsWrongType)
  {
    constexpr std::wstring_view kTestConfigFile =
        L"   GlobalBooleanSetting    =          12    \n" // Error
        L"AnotherGlobalBooleanSetting=55  \n"             // Error
        L"\n"
        L"; This is a comment\n"
        L"   ### This is another comment\n"
        L"   [Section1]   \n"
        L"IntegerSetting = false\n" // Error
        L"StringSetting =    string value  with    spaces that should be preserved except at the ends     \n"
        L"  [ Section2 ]  \n"
        L"IntegerSingleSetting = 1\n"
        L"IntegerSingleSetting = 2\n" // Error
        L"\n";
    TestConfigurationFileReader testConfigReader;
    const ConfigurationData configData =
        testConfigReader.ReadInMemoryConfigurationFile(kTestConfigFile);
    TEST_ASSERT(true == testConfigReader.HasErrorMessages());
    TEST_ASSERT(4 == testConfigReader.GetErrorMessages().size());
  }

  // Verifies that errors returned by the configuration file reader object itself are flagged as
  // errors. Sections that are rejected are skipped entirely, and values that are rejected are not
  // inserted into the configuration data object.
  TEST_CASE(Configuration_ConfigurationFileReader_ReadErrorsRejectedLines)
  {
    constexpr std::wstring_view kTestConfigFile =
        L"   GlobalBooleanSettingError    =          yes    \n" // Error
        L"AnotherGlobalBooleanSetting=no  \n"
        L"\n"
        L"; This is a comment\n"
        L"   ### This is another comment\n"
        L"   [Section1Error]   \n"    // Error - entire section should be skipped
        L"IntegerSettingError = 66\n" // Not counted as an error because this line should be skipped
        L"StringSetting =    string value  with    spaces that should be preserved except at the ends     \n"
        L"  [ Section2 ]  \n"
        L"IntegerMultiSettingError = 1\n" // Error
        L"IntegerMultiSetting = 2\n"
        L"IntegerMultiSetting = 3\n"
        L"IntegerMultiSetting = 4\n"
        L"IntegerMultiSetting = 5\n"
        L"     \n"
        L"     \n"
        L"\n";
    TestConfigurationFileReader testConfigReader;
    const ConfigurationData configData =
        testConfigReader.ReadInMemoryConfigurationFile(kTestConfigFile);
    TEST_ASSERT(true == testConfigReader.HasErrorMessages());
    TEST_ASSERT(3 == testConfigReader.GetErrorMessages().size());
    TEST_ASSERT(false == configData.Contains(L"Section1Error"));
    TEST_ASSERT(
        (true == configData.Contains(L" Section2 ")) &&
        (false == configData.Contains(L" Section2 ", L"IntegerMultiSettingError")));
  }

  // Verifies that the configuration file reader only queries for a mapping from type to value once
  // per value within each section.
  TEST_CASE(Configuration_ConfigurationFileReader_TypeForValueOncePerSectionAndName)
  {
    constexpr std::wstring_view kTestConfigFile =
        L"[Section1]\n"
        L"Value = 1\n"
        L"Value = 2\n"
        L"Value = 3\n"
        L"Value = 4\n"
        L"Value = 5\n"
        L"[Section2]\n"
        L"Value = aa\n"
        L"Value = bb\n"
        L"Value = cc\n"
        L"Value = dd\n"
        L"Value = ee\n";
    std::set<std::wstring, std::less<>> seenTypeForValueQueries;
    TestConfigurationFileReader testConfigReader(
        [&seenTypeForValueQueries](
            std::wstring_view section, std::wstring_view name) -> std::optional<EValueType>
        {
          TemporaryString queryIdentifier;
          queryIdentifier << section << L'/' << name;

          TEST_ASSERT(false == seenTypeForValueQueries.contains(queryIdentifier.AsStringView()));
          seenTypeForValueQueries.emplace(queryIdentifier.AsStringView());

          if (section == L"Section1")
            return EValueType::IntegerMultiValue;
          else if (section == L"Section2")
            return EValueType::StringMultiValue;
          else
            TEST_FAILED_BECAUSE(
                L"Unrecognized section name: \"%.*s\"",
                static_cast<int>(section.length()),
                section.data());
        });

    const ConfigurationData expectedConfigData(
        {{L"Section1", Section({{L"Value", {1, 2, 3, 4, 5}}})},
         {L"Section2", Section({{L"Value", {L"aa", L"bb", L"cc", L"dd", L"ee"}}})}});
    const ConfigurationData actualConfigData =
        testConfigReader.ReadInMemoryConfigurationFile(kTestConfigFile);
    TEST_ASSERT(actualConfigData == expectedConfigData);
    TEST_ASSERT(false == testConfigReader.HasErrorMessages());
  }

} // namespace CoreInfraTest
