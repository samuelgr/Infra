/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file ResolverTest.cpp
 *   Unit tests for resolution of named references contained within a string.
 **************************************************************************************************/

#include "Core/Resolver.h"

#include <optional>
#include <string>
#include <string_view>

#include "ApiWindows.h"

#include "Core/ProcessInfo.h"
#include "Core/Strings.h"
#include "Core/TemporaryBuffer.h"
#include "Test/TestCase.h"

namespace InfraTest
{
  using namespace ::Infra;

  /// Domain name used to exercise resolver functionality for custom domain resolution.
  constexpr std::wstring_view kStrReferenceDomainTesting = L"TESTING";

  /// Attempts to resolve an environment variable to a string.
  /// @param [in] name Environment variable name.
  /// @return Resolved string, if resolution succeeded.
  static std::optional<std::wstring> GetEnvironmentVariableString(std::wstring_view name)
  {
    TemporaryBuffer<wchar_t> environmentVariableValue;
    const DWORD getEnvironmentVariableResult = GetEnvironmentVariable(
        std::wstring(name).c_str(),
        environmentVariableValue.Data(),
        environmentVariableValue.Capacity());

    if ((getEnvironmentVariableResult >= environmentVariableValue.Capacity()) ||
        (0 == getEnvironmentVariableResult))
      return std::nullopt;

    return std::wstring(environmentVariableValue.Data());
  }

  /// Attempts to resolve a known path identifier to a string representation of its path.
  /// @param [in] knownFolder Known folder identifier.
  /// @return Resolved string, if resolution succeeded.
  static std::optional<std::wstring> GetKnownFolderPathString(const KNOWNFOLDERID& knownFolder)
  {
    std::optional<std::wstring> knownFolderPathString = std::nullopt;

    wchar_t* knownFolderPath = nullptr;
    const HRESULT getKnownFolderPathResult =
        SHGetKnownFolderPath(knownFolder, KF_FLAG_DEFAULT, NULL, &knownFolderPath);

    if (S_OK == getKnownFolderPathResult) knownFolderPathString = std::wstring(knownFolderPath);

    if (nullptr != knownFolderPath) CoTaskMemFree(knownFolderPath);

    return knownFolderPathString;
  }

  // Verifies that an environment variable can be resolved correctly in the nominal case that the
  // domain is explicitly specified.
  TEST_CASE(Resolver_SingleReference_EnvironmentVariable_Nominal)
  {
    constexpr std::wstring_view kEnvironmentVariableName = L"COMPUTERNAME";

    const std::optional<std::wstring> expectedResolveResult =
        GetEnvironmentVariableString(kEnvironmentVariableName);
    Resolver resolver;
    const ResolvedStringViewOrError actualResolveResult = resolver.ResolveSingleReference(
        std::wstring(kStrReferenceDomainEnvironmentVariable) +
        std::wstring(kStrDelimterReferenceDomainVsName) + std::wstring(kEnvironmentVariableName));

    TEST_ASSERT(true == expectedResolveResult.has_value());
    TEST_ASSERT(true == actualResolveResult.HasValue());
    TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult.value());
  }

  // Verifies that an environment variable can be resolved correctly when the domain is not
  // specified. Environment variables are the default domain.
  TEST_CASE(Resolver_SingleReference_EnvironmentVariable_DefaultDomain)
  {
    constexpr std::wstring_view kEnvironmentVariableName = L"COMPUTERNAME";

    const std::optional<std::wstring> expectedResolveResult =
        GetEnvironmentVariableString(kEnvironmentVariableName);
    Resolver resolver;
    const ResolvedStringViewOrError actualResolveResult =
        resolver.ResolveSingleReference(kEnvironmentVariableName);

    TEST_ASSERT(true == expectedResolveResult.has_value());
    TEST_ASSERT(true == actualResolveResult.HasValue());
    TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult.value());
  }

  // Verifies that an invalid environment variable fails to be resolved when the domain is
  // explicitly specified.
  TEST_CASE(Resolver_SingleReference_EnvironmentVariable_Invalid)
  {
    constexpr std::wstring_view kEnvironmentVariableName = L"ASDF=GH=JKL;";

    Resolver resolver;
    const ResolvedStringViewOrError actualResolveResult = resolver.ResolveSingleReference(
        std::wstring(kStrReferenceDomainEnvironmentVariable) +
        std::wstring(kStrDelimterReferenceDomainVsName) + std::wstring(kEnvironmentVariableName));
    TEST_ASSERT(true == actualResolveResult.HasError());
  }

  // Verifies that an invalid environment variable fails to be resolved when the domain is not
  // explicitly specified.
  TEST_CASE(Resolver_SingleReference_EnvironmentVariable_InvalidDefaultDomain)
  {
    constexpr std::wstring_view kEnvironmentVariableName = L"ASDF=GH=JKL;";

    Resolver resolver;
    const ResolvedStringViewOrError actualResolveResult =
        resolver.ResolveSingleReference(kEnvironmentVariableName);
    TEST_ASSERT(true == actualResolveResult.HasError());
  }

  // Verifies that known folder identifiers resolve correctly.
  // If the mapping is valid and results in a real path, the same should be true for reference
  // resolution. If not, then the reference resolution should also fail.
  TEST_CASE(Resolver_SingleReference_KnownFolderIdentifier_Nominal)
  {
    constexpr std::pair<std::wstring_view, const KNOWNFOLDERID*> kKnownFolderIdentifierRecords[] = {
        {L"AddNewPrograms", &FOLDERID_AddNewPrograms},
        {L"Desktop", &FOLDERID_Desktop},
        {L"Downloads", &FOLDERID_Downloads},
        {L"Fonts", &FOLDERID_Fonts},
        {L"HomeGroupCurrentUser", &FOLDERID_HomeGroupCurrentUser},
        {L"InternetCache", &FOLDERID_InternetCache},
        {L"NetworkFolder", &FOLDERID_NetworkFolder},
        {L"Pictures", &FOLDERID_Pictures},
        {L"Profile", &FOLDERID_Profile},
        {L"RecycleBinFolder", &FOLDERID_RecycleBinFolder},
        {L"RoamingAppData", &FOLDERID_RoamingAppData},
        {L"SavedGames", &FOLDERID_SavedGames},
        {L"Windows", &FOLDERID_Windows}};

    const std::wstring testInputPrefix = std::wstring(kStrReferenceDomainKnownFolderIdentifier) +
        std::wstring(kStrDelimterReferenceDomainVsName);

    for (const auto& knownFolderIdentifierRecord : kKnownFolderIdentifierRecords)
    {
      const std::wstring knownFolderInputString =
          testInputPrefix + std::wstring(knownFolderIdentifierRecord.first);
      const KNOWNFOLDERID& knownFolderIdentifier = *knownFolderIdentifierRecord.second;

      const std::optional<std::wstring> expectedResolveResult =
          GetKnownFolderPathString(knownFolderIdentifier);
      Resolver resolver;
      const ResolvedStringViewOrError actualResolveResult =
          resolver.ResolveSingleReference(knownFolderInputString);

      TEST_ASSERT(actualResolveResult.HasValue() == expectedResolveResult.has_value());

      if (true == expectedResolveResult.has_value())
        TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult.value());
    }
  }

  // Verifies that invalid known folder identifiers fail to resolve.
  // Inputs are as above but with case modifications and leading or trailing whitespace.
  TEST_CASE(Resolver_SingleReference_KnownFolderIdentifier_Invalid)
  {
    constexpr std::pair<std::wstring_view, const KNOWNFOLDERID*> kKnownFolderIdentifierRecords[] = {
        {L" desktop", &FOLDERID_Desktop},
        {L"Downloads ", &FOLDERID_Downloads},
        {L"  Fonts  ", &FOLDERID_Fonts},
        {L" InternetCache", &FOLDERID_InternetCache},
        {L"\tWindows", &FOLDERID_Windows}};

    const std::wstring testInputPrefix = std::wstring(kStrReferenceDomainKnownFolderIdentifier) +
        std::wstring(kStrDelimterReferenceDomainVsName);

    for (const auto& knownFolderIdentifierRecord : kKnownFolderIdentifierRecords)
    {
      const std::wstring knownFolderInputString =
          testInputPrefix + std::wstring(knownFolderIdentifierRecord.first);

      Resolver resolver;
      const ResolvedStringViewOrError actualResolveResult =
          resolver.ResolveSingleReference(knownFolderInputString);
      TEST_ASSERT(true == actualResolveResult.HasError());
    }
  }

  // Verifies that a custom domain definition can be resolved correctly in the nominal case of no
  // embedded references.
  TEST_CASE(Resolver_SingleReference_CustomDomainDefinition_Nominal)
  {
    const std::wstring kVariableName = L"W";
    const std::wstring kVariableValue = L"This is the evaluated value of W.";

    Resolver resolver;
    resolver.RegisterCustomDomain(kStrReferenceDomainTesting, {{kVariableName, kVariableValue}});

    const std::wstring_view expectedResolveResult = kVariableValue;
    const ResolvedStringViewOrError actualResolveResult = resolver.ResolveSingleReference(
        std::wstring(kStrReferenceDomainTesting) + std::wstring(kStrDelimterReferenceDomainVsName) +
        std::wstring(kVariableName));

    TEST_ASSERT(true == actualResolveResult.HasValue());
    TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult);
  }

  // Verifies that a custom domain definition can be resolved correctly in the more complex case of
  // embedded references.
  TEST_CASE(Resolver_SingleReference_CustomDomainDefinition_Embedded)
  {
    Resolver resolver;
    resolver.RegisterCustomDomain(
        kStrReferenceDomainTesting,
        {{L"X", L"Value of X"},
         {L"Y", L"Value of Y incorporates value of X: (%TESTING::X%)"},
         {L"Z", L"Value of Z incorporates value of Y: (%TESTING::Y%)"}});

    const std::wstring_view expectedResolveResult =
        L"Value of Z incorporates value of Y: (Value of Y incorporates value of X: (Value of X))";
    const ResolvedStringViewOrError actualResolveResult = resolver.ResolveSingleReference(
        std::wstring(kStrReferenceDomainTesting) + std::wstring(kStrDelimterReferenceDomainVsName) +
        L"Z");

    TEST_ASSERT(true == actualResolveResult.HasValue());
    TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult);
  }

  // Verifies that a custom domain definition fails to resolve when it references itself.
  TEST_CASE(Resolver_SingleReference_CustomDomainDefinition_EmbeddedCircularSingle)
  {
    const std::wstring kVariableName = L"Invalid";
    const std::wstring kVariableValue = L"This is the evaluated value of %TESTING::Invalid%.";

    Resolver resolver;
    resolver.RegisterCustomDomain(kStrReferenceDomainTesting, {{kVariableName, kVariableValue}});

    const ResolvedStringViewOrError actualResolveResult = resolver.ResolveSingleReference(
        std::wstring(kStrReferenceDomainTesting) + std::wstring(kStrDelimterReferenceDomainVsName) +
        std::wstring(kVariableName));
    TEST_ASSERT(true == actualResolveResult.HasError());
  }

  // Verifies that a custom domain definition fails to resolve when there is a cycle across multiple
  // references.
  TEST_CASE(Resolver_SingleReference_CustomDomainDefinition_EmbeddedCircularMultiple)
  {
    Resolver resolver;
    resolver.RegisterCustomDomain(
        kStrReferenceDomainTesting,
        {{L"Invalid1", L"Value of %TESTING::Invalid2%"},
         {L"Invalid2", L"Value of Invalid2 incorporates %TESTING::Invalid3%"},
         {L"Invalid3", L"Value of Invalid3 incorporates %TESTING::Invalid1%"}});

    const ResolvedStringViewOrError actualResolveResult = resolver.ResolveSingleReference(
        std::wstring(kStrReferenceDomainTesting) + std::wstring(kStrDelimterReferenceDomainVsName) +
        L"Invalid2");
    TEST_ASSERT(true == actualResolveResult.HasError());
  }

  // Verifies that a custom domain definition referencing an unrecognized variable fails to be
  // resolved.
  TEST_CASE(Resolver_SingleReference_CustomDomainDefinition_Invalid)
  {
    Resolver resolver;
    resolver.RegisterCustomDomain(kStrReferenceDomainTesting, {{L"X", L"Value of X"}});

    const ResolvedStringViewOrError actualResolveResult = resolver.ResolveSingleReference(
        std::wstring(kStrReferenceDomainTesting) + std::wstring(kStrDelimterReferenceDomainVsName) +
        L"UnknownVariable123456");
    TEST_ASSERT(true == actualResolveResult.HasError());
  }

  // Verifies that valid references to built-in strings are resolved correctly and without regard
  // for case.
  TEST_CASE(Resolver_SingleReference_Builtin_Nominal)
  {
    const std::pair<std::wstring_view, std::wstring_view> kBuiltinStringTestRecords[] = {
        {L"BUILTIN::ExecutableBaseName", ProcessInfo::GetExecutableBaseName()},
        {L"builtin::executabledirectoryname", ProcessInfo::GetExecutableDirectoryName()},
        {L"BUILTIN::CoreInfraDirectoryName", ProcessInfo::GetThisModuleDirectoryName()},
    };

    for (const auto& kBuiltinStringTestRecord : kBuiltinStringTestRecords)
    {
      const std::wstring_view expectedResolveResult = kBuiltinStringTestRecord.second;
      Resolver resolver;
      const ResolvedStringViewOrError actualResolveResult =
          resolver.ResolveSingleReference(kBuiltinStringTestRecord.first);

      TEST_ASSERT(true == actualResolveResult.HasValue());
      TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult);
    }
  }

  // Verifies that invalid inputs for single-reference resolution cause the resolution to fail.
  // This could be unrecognized domains or unparseable strings.
  TEST_CASE(Resolver_SingleReference_Invalid)
  {
    constexpr std::wstring_view kInvalidInputStrings[] = {
        L"INVALIDDOMAIN::SomeVariable",
        L"ENV::COMPUTERNAME::",
        L"ENV::COMPUTERNAME::extrastuff",
        L"::ENV::COMPUTERNAME",
        L"::",
        L""};

    Resolver resolver;
    for (const auto& kInvalidInputString : kInvalidInputStrings)
      TEST_ASSERT(true == resolver.ResolveSingleReference(kInvalidInputString).HasError());
  }

  // Verifies that valid inputs for all-reference resolution produce the correct successful
  // resolution results. No escape characters are supplied.
  TEST_CASE(Resolver_AllReferences_Nominal)
  {
    Resolver resolver;
    resolver.RegisterCustomDomain(
        kStrReferenceDomainTesting,
        {{L"BaseDir", L"%FOLDERID::SavedGames%"}, {L"PercentageComplete", L"56.789"}});

    const std::pair<std::wstring_view, std::wstring> kAllReferenceTestRecords[] = {
        {L"Selected base directory: %TESTING::BaseDir%",
         std::wstring(L"Selected base directory: ") +
             GetKnownFolderPathString(FOLDERID_SavedGames).value()},
        {L"You are %TESTING::PercentageComplete%%% done!", L"You are 56.789% done!"},
        {L"System is %TESTING::PercentageComplete%%% ready to provide your files in %TESTING::BaseDir%.",
         std::wstring(L"System is 56.789% ready to provide your files in ") +
             GetKnownFolderPathString(FOLDERID_SavedGames).value() + std::wstring(L".")},
        {L"%%%%%%::%%%%::::%%%%", L"%%%::%%::::%%"}};

    for (const auto& kAllReferenceTestRecord : kAllReferenceTestRecords)
    {
      const std::wstring_view expectedResolveResult = kAllReferenceTestRecord.second;
      const ResolvedStringOrError actualResolveResult =
          resolver.ResolveAllReferences(kAllReferenceTestRecord.first);
      TEST_ASSERT(true == actualResolveResult.HasValue());
      TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult);
    }
  }

  // Verifies that valid inputs for all-reference resolution produce the correct successful
  // resolution results. Multiple escape characters are supplied and the default escape sequence
  // is used.
  TEST_CASE(Resolver_AllReferences_EscapeSequenceDefault)
  {
    Resolver resolver;
    resolver.RegisterCustomDomain(
        kStrReferenceDomainTesting,
        {{L"Variable1", L"abcdef"},
         {L"Variable2", L"ABCDEF"},
         {L"Variable3", L"This is a NICE test for real!"},
         {L"Variable4", L" c F "}});

    constexpr std::wstring_view kEscapeCharacters = L"cF ";

    // For most of these inputs first the literal value of the reference appears and then the
    // reference itself. This is to ensure that only the reference result gets escaped, not the
    // literal, even if the literal contains special characters marked for escaping.
    const std::pair<std::wstring_view, std::wstring> kAllReferenceTestRecords[] = {
        {L"abcdef %TESTING::Variable1%",
         L"abcdef "
         L"ab\\cdef"},
        {L"ABCDEF %TESTING::Variable2%",
         L"ABCDEF "
         L"ABCDE\\F"},
        {L"This is a NICE test for real! %TESTING::Variable3%",
         L"This is a NICE test for real! "
         L"This\\ is\\ a\\ NICE\\ test\\ for\\ real!"},
        {L"%TESTING::Variable4%", L"\\ \\c\\ \\F\\ "}};

    for (const auto& kAllReferenceTestRecord : kAllReferenceTestRecords)
    {
      const std::wstring_view expectedResolveResult = kAllReferenceTestRecord.second;
      const ResolvedStringOrError actualResolveResult =
          resolver.ResolveAllReferences(kAllReferenceTestRecord.first, kEscapeCharacters);

      TEST_ASSERT(true == actualResolveResult.HasValue());
      TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult);
    }
  }

  // Verifies that valid inputs for all-reference resolution produce the correct successful
  // resolution results. Multiple escape characters are supplied along with special sequences for
  // the start and end of escape sequences.
  TEST_CASE(Resolver_AllReferences_EscapeSequenceStartAndEnd)
  {
    Resolver resolver;
    resolver.RegisterCustomDomain(
        kStrReferenceDomainTesting,
        {{L"Variable5", L"abcdef"},
         {L"Variable6", L"ABCDEF"},
         {L"Variable7", L"This is a NICE test for real!"},
         {L"Variable8", L" c F "}});

    constexpr std::wstring_view kEscapeCharacters = L"cF ";
    constexpr std::wstring_view kEscapeSequenceStart = L"!&!";
    constexpr std::wstring_view kEscapeSequenceEnd = L">>";

    // For most of these inputs first the literal value of the reference appears and then the
    // reference itself. This is to ensure that only the reference result gets escaped, not the
    // literal, even if the literal contains special characters marked for escaping.
    const std::pair<std::wstring_view, std::wstring> kAllReferenceTestRecords[] = {
        {L"abcdef %TESTING::Variable5%",
         L"abcdef "
         L"ab!&!c>>def"},
        {L"ABCDEF %TESTING::Variable6%",
         L"ABCDEF "
         L"ABCDE!&!F>>"},
        {L"This is a NICE test for real! %TESTING::Variable7%",
         L"This is a NICE test for real! "
         L"This!&! >>is!&! >>a!&! >>NICE!&! >>test!&! >>for!&! >>real!"},
        {L"%TESTING::Variable8%", L"!&! >>!&!c>>!&! >>!&!F>>!&! >>"}};

    for (const auto& kAllReferenceTestRecord : kAllReferenceTestRecords)
    {
      const std::wstring_view expectedResolveResult = kAllReferenceTestRecord.second;
      const ResolvedStringOrError actualResolveResult = resolver.ResolveAllReferences(
          kAllReferenceTestRecord.first,
          kEscapeCharacters,
          kEscapeSequenceStart,
          kEscapeSequenceEnd);

      TEST_ASSERT(true == actualResolveResult.HasValue());
      TEST_ASSERT(actualResolveResult.Value() == expectedResolveResult);
    }
  }

  // Verifies that invalid inputs for all-reference resolution cause the resolution to fail.
  TEST_CASE(Resolver_AllReferences_Invalid)
  {
    Resolver resolver;
    resolver.RegisterCustomDomain(
        kStrReferenceDomainTesting,
        {{L"BaseDir", L"%FOLDERID::TotallyUnrecognizedFolderIdentifier%"}});

    constexpr std::wstring_view kInvalidInputStrings[] = {
        L"Using computer %COMPUTERNAME% as user %USERNAME%. There is an extra % sign at the end that is not matched.",
        L"Using computer %COMPUTERNAME% as user %TESTING::InvalidReference%.",
        L"Selected base directory: %TESTING::BaseDir%",
        L"%%%"};

    for (const auto& kInvalidInputString : kInvalidInputStrings)
      TEST_ASSERT(true == resolver.ResolveAllReferences(kInvalidInputString).HasError());
  }
} // namespace InfraTest
