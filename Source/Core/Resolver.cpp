/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file Resolver.cpp
 *   Implementation of functions for resolving named references contained in strings.
 **************************************************************************************************/

#include "Core/Resolver.h"

#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include "ApiWindows.h"

#include "Core/Configuration.h"
#include "Core/DebugAssert.h"
#include "Core/ProcessInfo.h"
#include "Core/Strings.h"
#include "Core/SystemInfo.h"
#include "Core/TemporaryBuffer.h"

namespace Infra
{
  /// Default reference domain to use when none are specified.
  static constexpr std::wstring_view kReferenceDefaultDomain =
      kStrReferenceDomainEnvironmentVariable;

  /// Resolves a built-in string reference.
  /// Built-in strings are a subset of the "Strings.h" declarations.
  /// @param [in] name Name of the built-in string to resolve.
  /// @return Resolved value on success, error message on failure.
  static ResolvedStringOrError ResolveBuiltin(std::wstring_view name)
  {
    static const struct
    {
      std::wstring productCompleteFilename;
      std::wstring productBaseName;
      std::wstring productDirectoryName;
    } kBuiltinProductKeyStrings = {
        .productCompleteFilename = std::wstring(Strings::Format(
            L"%.*sCompleteFilename",
            static_cast<int>(ProcessInfo::GetProductName().length()),
            ProcessInfo::GetProductName().data())),
        .productBaseName = std::wstring(Strings::Format(
            L"%.*sBaseName",
            static_cast<int>(ProcessInfo::GetProductName().length()),
            ProcessInfo::GetProductName().data())),
        .productDirectoryName = std::wstring(Strings::Format(
            L"%.*sDirectoryName",
            static_cast<int>(ProcessInfo::GetProductName().length()),
            ProcessInfo::GetProductName().data()))};

    static const std::unordered_map<
        std::wstring_view,
        std::wstring_view,
        Strings::CaseInsensitiveHasher<wchar_t>,
        Strings::CaseInsensitiveEqualityComparator<wchar_t>>
        kBuiltinStrings = {
            {kBuiltinProductKeyStrings.productCompleteFilename,
             ProcessInfo::GetThisModuleCompleteFilename()},
            {kBuiltinProductKeyStrings.productBaseName, ProcessInfo::GetThisModuleBaseName()},
            {kBuiltinProductKeyStrings.productDirectoryName,
             ProcessInfo::GetThisModuleDirectoryName()},
            {L"ExecutableCompleteFilename", ProcessInfo::GetExecutableCompleteFilename()},
            {L"ExecutableBaseName", ProcessInfo::GetExecutableBaseName()},
            {L"ExecutableDirectoryName", ProcessInfo::GetExecutableDirectoryName()},
            {L"NetBiosHostname", SystemInfo::GetNetBiosHostname()},
            {L"DnsHostname", SystemInfo::GetDnsHostname()},
            {L"DnsDomain", SystemInfo::GetDnsDomain()},
            {L"DnsFullyQualified", SystemInfo::GetDnsFullyQualified()}};

    const auto builtinStringsIter = kBuiltinStrings.find(name);
    if (kBuiltinStrings.cend() == builtinStringsIter)
      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Unrecognized built-in string", static_cast<int>(name.length()), name.data()));

    return ResolvedStringOrError::MakeValue(builtinStringsIter->second);
  }

  /// Resolves an environment variable.
  /// @param [in] name Name of the environment variable to resolve.
  /// @return Resolved value on success, error message on failure.
  static ResolvedStringOrError ResolveEnvironmentVariable(std::wstring_view name)
  {
    TemporaryBuffer<wchar_t> environmentVariableValue;
    const DWORD getEnvironmentVariableResult = GetEnvironmentVariable(
        TemporaryString(name).AsCString(),
        environmentVariableValue.Data(),
        environmentVariableValue.Capacity());

    if (getEnvironmentVariableResult >= environmentVariableValue.Capacity())
      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Failed to obtain environment variable value: Value is too long",
          static_cast<int>(name.length()),
          name.data()));
    else if (0 == getEnvironmentVariableResult)
      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Failed to obtain environment variable value: %s",
          static_cast<int>(name.length()),
          name.data(),
          Strings::FromSystemErrorCode(GetLastError()).AsCString()));

    return ResolvedStringOrError::MakeValue(environmentVariableValue.Data());
  }

  /// Resolves a known folder identifier.
  /// Known folder identifier names are documented at
  /// https://docs.microsoft.com/en-us/windows/win32/shell/knownfolderid and should be passed
  /// without the FOLDERID prefix.
  /// @param [in] name Name of the known folder identifier to resolve.
  /// @return Resolved value on success, error message on failure.
  static ResolvedStringOrError ResolveKnownFolderIdentifier(std::wstring_view name)
  {
    // Every single known folder definition from "KnownFolders.h" is contained in this map.
    // Some of them are virtual and so cannot be mapped to real paths.
    static const std::unordered_map<
        std::wstring_view,
        const KNOWNFOLDERID*,
        Strings::CaseInsensitiveHasher<wchar_t>,
        Strings::CaseInsensitiveEqualityComparator<wchar_t>>
        kKnownFolderIdentifiers = {
            {L"NetworkFolder", &FOLDERID_NetworkFolder},
            {L"ComputerFolder", &FOLDERID_ComputerFolder},
            {L"InternetFolder", &FOLDERID_InternetFolder},
            {L"ControlPanelFolder", &FOLDERID_ControlPanelFolder},
            {L"PrintersFolder", &FOLDERID_PrintersFolder},
            {L"SyncManagerFolder", &FOLDERID_SyncManagerFolder},
            {L"SyncSetupFolder", &FOLDERID_SyncSetupFolder},
            {L"ConflictFolder", &FOLDERID_ConflictFolder},
            {L"SyncResultsFolder", &FOLDERID_SyncResultsFolder},
            {L"RecycleBinFolder", &FOLDERID_RecycleBinFolder},
            {L"ConnectionsFolder", &FOLDERID_ConnectionsFolder},
            {L"Fonts", &FOLDERID_Fonts},
            {L"Desktop", &FOLDERID_Desktop},
            {L"Startup", &FOLDERID_Startup},
            {L"Programs", &FOLDERID_Programs},
            {L"StartMenu", &FOLDERID_StartMenu},
            {L"Recent", &FOLDERID_Recent},
            {L"SendTo", &FOLDERID_SendTo},
            {L"Documents", &FOLDERID_Documents},
            {L"Favorites", &FOLDERID_Favorites},
            {L"NetHood", &FOLDERID_NetHood},
            {L"PrintHood", &FOLDERID_PrintHood},
            {L"Templates", &FOLDERID_Templates},
            {L"CommonStartup", &FOLDERID_CommonStartup},
            {L"CommonPrograms", &FOLDERID_CommonPrograms},
            {L"CommonStartMenu", &FOLDERID_CommonStartMenu},
            {L"PublicDesktop", &FOLDERID_PublicDesktop},
            {L"ProgramData", &FOLDERID_ProgramData},
            {L"CommonTemplates", &FOLDERID_CommonTemplates},
            {L"PublicDocuments", &FOLDERID_PublicDocuments},
            {L"RoamingAppData", &FOLDERID_RoamingAppData},
            {L"LocalAppData", &FOLDERID_LocalAppData},
            {L"LocalAppDataLow", &FOLDERID_LocalAppDataLow},
            {L"InternetCache", &FOLDERID_InternetCache},
            {L"Cookies", &FOLDERID_Cookies},
            {L"History", &FOLDERID_History},
            {L"System", &FOLDERID_System},
            {L"SystemX86", &FOLDERID_SystemX86},
            {L"Windows", &FOLDERID_Windows},
            {L"Profile", &FOLDERID_Profile},
            {L"Pictures", &FOLDERID_Pictures},
            {L"ProgramFilesX86", &FOLDERID_ProgramFilesX86},
            {L"ProgramFilesCommonX86", &FOLDERID_ProgramFilesCommonX86},
            {L"ProgramFilesX64", &FOLDERID_ProgramFilesX64},
            {L"ProgramFilesCommonX64", &FOLDERID_ProgramFilesCommonX64},
            {L"ProgramFiles", &FOLDERID_ProgramFiles},
            {L"ProgramFilesCommon", &FOLDERID_ProgramFilesCommon},
            {L"UserProgramFiles", &FOLDERID_UserProgramFiles},
            {L"UserProgramFilesCommon", &FOLDERID_UserProgramFilesCommon},
            {L"AdminTools", &FOLDERID_AdminTools},
            {L"CommonAdminTools", &FOLDERID_CommonAdminTools},
            {L"Music", &FOLDERID_Music},
            {L"Videos", &FOLDERID_Videos},
            {L"Ringtones", &FOLDERID_Ringtones},
            {L"PublicPictures", &FOLDERID_PublicPictures},
            {L"PublicMusic", &FOLDERID_PublicMusic},
            {L"PublicVideos", &FOLDERID_PublicVideos},
            {L"PublicRingtones", &FOLDERID_PublicRingtones},
            {L"ResourceDir", &FOLDERID_ResourceDir},
            {L"LocalizedResourcesDir", &FOLDERID_LocalizedResourcesDir},
            {L"CommonOEMLinks", &FOLDERID_CommonOEMLinks},
            {L"CDBurning", &FOLDERID_CDBurning},
            {L"UserProfiles", &FOLDERID_UserProfiles},
            {L"Playlists", &FOLDERID_Playlists},
            {L"SamplePlaylists", &FOLDERID_SamplePlaylists},
            {L"SampleMusic", &FOLDERID_SampleMusic},
            {L"SamplePictures", &FOLDERID_SamplePictures},
            {L"SampleVideos", &FOLDERID_SampleVideos},
            {L"PhotoAlbums", &FOLDERID_PhotoAlbums},
            {L"Public", &FOLDERID_Public},
            {L"ChangeRemovePrograms", &FOLDERID_ChangeRemovePrograms},
            {L"AppUpdates", &FOLDERID_AppUpdates},
            {L"AddNewPrograms", &FOLDERID_AddNewPrograms},
            {L"Downloads", &FOLDERID_Downloads},
            {L"PublicDownloads", &FOLDERID_PublicDownloads},
            {L"SavedSearches", &FOLDERID_SavedSearches},
            {L"QuickLaunch", &FOLDERID_QuickLaunch},
            {L"Contacts", &FOLDERID_Contacts},
            {L"SidebarParts", &FOLDERID_SidebarParts},
            {L"SidebarDefaultParts", &FOLDERID_SidebarDefaultParts},
            {L"PublicGameTasks", &FOLDERID_PublicGameTasks},
            {L"GameTasks", &FOLDERID_GameTasks},
            {L"SavedGames", &FOLDERID_SavedGames},
            {L"Games", &FOLDERID_Games},
            {L"SEARCH_MAPI", &FOLDERID_SEARCH_MAPI},
            {L"SEARCH_CSC", &FOLDERID_SEARCH_CSC},
            {L"Links", &FOLDERID_Links},
            {L"UsersFiles", &FOLDERID_UsersFiles},
            {L"UsersLibraries", &FOLDERID_UsersLibraries},
            {L"SearchHome", &FOLDERID_SearchHome},
            {L"OriginalImages", &FOLDERID_OriginalImages},
            {L"DocumentsLibrary", &FOLDERID_DocumentsLibrary},
            {L"MusicLibrary", &FOLDERID_MusicLibrary},
            {L"PicturesLibrary", &FOLDERID_PicturesLibrary},
            {L"VideosLibrary", &FOLDERID_VideosLibrary},
            {L"RecordedTVLibrary", &FOLDERID_RecordedTVLibrary},
            {L"HomeGroup", &FOLDERID_HomeGroup},
            {L"HomeGroupCurrentUser", &FOLDERID_HomeGroupCurrentUser},
            {L"DeviceMetadataStore", &FOLDERID_DeviceMetadataStore},
            {L"Libraries", &FOLDERID_Libraries},
            {L"PublicLibraries", &FOLDERID_PublicLibraries},
            {L"UserPinned", &FOLDERID_UserPinned},
            {L"ImplicitAppShortcuts", &FOLDERID_ImplicitAppShortcuts},
            {L"AccountPictures", &FOLDERID_AccountPictures},
            {L"PublicUserTiles", &FOLDERID_PublicUserTiles},
            {L"AppsFolder", &FOLDERID_AppsFolder},
            {L"StartMenuAllPrograms", &FOLDERID_StartMenuAllPrograms},
            {L"CommonStartMenuPlaces", &FOLDERID_CommonStartMenuPlaces},
            {L"ApplicationShortcuts", &FOLDERID_ApplicationShortcuts},
            {L"RoamingTiles", &FOLDERID_RoamingTiles},
            {L"RoamedTileImages", &FOLDERID_RoamedTileImages},
            {L"Screenshots", &FOLDERID_Screenshots},
            {L"CameraRoll", &FOLDERID_CameraRoll},
            {L"SkyDrive", &FOLDERID_SkyDrive},
            {L"OneDrive", &FOLDERID_OneDrive},
            {L"SkyDriveDocuments", &FOLDERID_SkyDriveDocuments},
            {L"SkyDrivePictures", &FOLDERID_SkyDrivePictures},
            {L"SkyDriveMusic", &FOLDERID_SkyDriveMusic},
            {L"SkyDriveCameraRoll", &FOLDERID_SkyDriveCameraRoll},
            {L"SearchHistory", &FOLDERID_SearchHistory},
            {L"SearchTemplates", &FOLDERID_SearchTemplates},
            {L"CameraRollLibrary", &FOLDERID_CameraRollLibrary},
            {L"SavedPictures", &FOLDERID_SavedPictures},
            {L"SavedPicturesLibrary", &FOLDERID_SavedPicturesLibrary},
            {L"RetailDemo", &FOLDERID_RetailDemo},
            {L"Device", &FOLDERID_Device},
            {L"DevelopmentFiles", &FOLDERID_DevelopmentFiles},
            {L"Objects3D", &FOLDERID_Objects3D},
            {L"AppCaptures", &FOLDERID_AppCaptures},
            {L"LocalDocuments", &FOLDERID_LocalDocuments},
            {L"LocalPictures", &FOLDERID_LocalPictures},
            {L"LocalVideos", &FOLDERID_LocalVideos},
            {L"LocalMusic", &FOLDERID_LocalMusic},
            {L"LocalDownloads", &FOLDERID_LocalDownloads},
            {L"RecordedCalls", &FOLDERID_RecordedCalls},
            {L"AllAppMods", &FOLDERID_AllAppMods},
            {L"CurrentAppMods", &FOLDERID_CurrentAppMods},
            {L"AppDataDesktop", &FOLDERID_AppDataDesktop},
            {L"AppDataDocuments", &FOLDERID_AppDataDocuments},
            {L"AppDataFavorites", &FOLDERID_AppDataFavorites},
            {L"AppDataProgramData", &FOLDERID_AppDataProgramData},
            {L"LocalStorage", &FOLDERID_LocalStorage}};

    const auto knownFolderIter = kKnownFolderIdentifiers.find(name);
    if (kKnownFolderIdentifiers.cend() == knownFolderIter)
      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Unrecognized known folder identifier",
          static_cast<int>(name.length()),
          name.data()));

    wchar_t* knownFolderPath = nullptr;
    const HRESULT getKnownFolderPathResult =
        SHGetKnownFolderPath(*knownFolderIter->second, KF_FLAG_DEFAULT, NULL, &knownFolderPath);

    if (S_OK != getKnownFolderPathResult)
    {
      if (nullptr != knownFolderPath) CoTaskMemFree(knownFolderPath);

      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Failed to obtain known folder path: error code 0x%08lx",
          static_cast<int>(name.length()),
          name.data(),
          static_cast<unsigned long>(getKnownFolderPathResult)));
    }
    else
    {
      std::wstring knownFolderPathString(knownFolderPath);
      if (nullptr != knownFolderPath) CoTaskMemFree(knownFolderPath);
      return ResolvedStringOrError::MakeValue(std::move(knownFolderPathString));
    }
  }

  /// Creates a fully-qualified reference name from a domain and a variable.
  /// @param [in] domain Domain part of the fully-qualified reference.
  /// @param [in] name Name part of the fully-qualified reference.
  /// @return String that represents the fully-qualified reference of the name within its domain.
  static std::wstring FullyQualifiedReferenceFromParts(
      std::wstring_view domain, std::wstring_view name)
  {
    std::wstring referenceFullyQualified;
    referenceFullyQualified.reserve(
        1 + domain.length() + kStrDelimterReferenceDomainVsName.length() + name.length());
    referenceFullyQualified += domain;
    referenceFullyQualified += kStrDelimterReferenceDomainVsName;
    referenceFullyQualified += name;
    return referenceFullyQualified;
  }

  Resolver::Resolver(void)
      : resolutionsInProgress(),
        resolvedSingleReferenceCache(),
        resolversByDomain(
            {{kStrReferenceDomainBuiltin, &ResolveBuiltin},
             {kStrReferenceDomainEnvironmentVariable, &ResolveEnvironmentVariable},
             {kStrReferenceDomainKnownFolderIdentifier, &ResolveKnownFolderIdentifier}})
  {}

  bool Resolver::RegisterCustomDomain(std::wstring_view domain, TDefinitions&& definitions)
  {
    if (true == domain.empty()) return false;
    return resolversByDomain
        .emplace(
            domain,
            [this, definitions = std::move(definitions)](
                std::wstring_view name) -> ResolvedStringOrError
            {
              return ResolveCustomDomainVariable(name, definitions);
            })
        .second;
  }

  ResolvedStringOrError Resolver::ResolveCustomDomainVariable(
      std::wstring_view name, const TDefinitions& definitions)
  {
    const auto definitionIter = definitions.find(name);
    if (definitions.cend() == definitionIter)
      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Unrecognized variable name", static_cast<int>(name.length()), name.data()));
    ResolvedStringOrError resolvedDefinition = ResolveAllReferences(definitionIter->second);
    return resolvedDefinition;
  }

  ResolvedStringViewOrError Resolver::ResolveSingleReference(std::wstring_view str)
  {
    const auto previouslyResolvedIter = resolvedSingleReferenceCache.find(str);
    if (resolvedSingleReferenceCache.cend() != previouslyResolvedIter)
      return ResolvedStringViewOrError::MakeValue(previouslyResolvedIter->second);

    TemporaryVector<std::wstring_view> strParts =
        Strings::Split(str, kStrDelimterReferenceDomainVsName);
    std::wstring_view strPartReferenceDomain;
    std::wstring_view strPartReferenceName;

    switch (strParts.Size())
    {
      case 1:
        strPartReferenceDomain = kReferenceDefaultDomain;
        strPartReferenceName = strParts[0];
        break;

      case 2:
        strPartReferenceDomain = strParts[0];
        strPartReferenceName = strParts[1];
        break;

      default:
        return ResolvedStringViewOrError::MakeError(Strings::Format(
            L"%.*s: Unparseable reference", static_cast<int>(str.length()), str.data()));
    }

    const auto resolverByDomainIter = resolversByDomain.find(strPartReferenceDomain);
    if (resolversByDomain.cend() == resolverByDomainIter)
      return ResolvedStringViewOrError::MakeError(Strings::Format(
          L"%.*s: Unrecognized reference domain",
          static_cast<int>(strPartReferenceDomain.length()),
          strPartReferenceDomain.data()));

    std::pair resolutionInProgress = resolutionsInProgress.emplace(
        FullyQualifiedReferenceFromParts(strPartReferenceDomain, strPartReferenceName));
    if (false == resolutionInProgress.second)
      return ResolvedStringViewOrError::MakeError(
          Strings::Format(L"%s: Circular reference", resolutionInProgress.first->c_str()));
    ResolvedStringOrError resolveResult = resolverByDomainIter->second(strPartReferenceName);
    resolutionsInProgress.erase(resolutionInProgress.first);

    if (true == resolveResult.HasValue())
      return ResolvedStringViewOrError::MakeValue(
          resolvedSingleReferenceCache.emplace(str, resolveResult.Value()).first->second);
    else
      return ResolvedStringViewOrError::MakeError(std::move(resolveResult.Error()));
  }

  ResolvedStringOrError Resolver::ResolveAllReferences(
      std::wstring_view str,
      std::wstring_view escapeCharacters,
      std::wstring_view escapeSequenceStart,
      std::wstring_view escapeSequenceEnd)
  {
    TemporaryString resolvedStr;
    TemporaryVector<std::wstring_view> strParts =
        Strings::Split(str, kStrDelimiterReferenceVsLiteral);

    if (1 != (strParts.Size() % 2))
      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Unmatched '%.*s' delimiters",
          static_cast<int>(str.length()),
          str.data(),
          static_cast<int>(kStrDelimiterReferenceVsLiteral.length()),
          kStrDelimiterReferenceVsLiteral.data()));

    resolvedStr << strParts[0];

    for (unsigned int i = 1; i < strParts.Size(); i += 2)
    {
      if (true == strParts[i].empty())
      {
        resolvedStr << kStrDelimiterReferenceVsLiteral;
      }
      else
      {
        const ResolvedStringViewOrError resolvedReferenceResult =
            ResolveSingleReference(strParts[i]);

        if (true == resolvedReferenceResult.HasError())
          return ResolvedStringOrError::MakeError(Strings::Format(
              L"%.*s: Failed to resolve reference: %s",
              static_cast<int>(str.length()),
              str.data(),
              resolvedReferenceResult.Error().AsCString()));

        if (true == escapeCharacters.empty())
        {
          resolvedStr << resolvedReferenceResult.Value();
        }
        else
        {
          for (const auto resolvedReferenceResultChar : resolvedReferenceResult.Value())
          {
            if (true == escapeCharacters.contains(resolvedReferenceResultChar))
            {
              resolvedStr << escapeSequenceStart;
              resolvedStr << resolvedReferenceResultChar;
              resolvedStr << escapeSequenceEnd;
            }
            else
            {
              resolvedStr << resolvedReferenceResultChar;
            }
          }
        }
      }

      resolvedStr << strParts[i + 1];
    }

    if (true == resolvedStr.Overflow())
      return ResolvedStringOrError::MakeError(Strings::Format(
          L"%.*s: Successfully resolved, but result exceeds the limit of %u characters",
          static_cast<int>(str.length()),
          str.data(),
          resolvedStr.Capacity()));

    return ResolvedStringOrError::MakeValue(resolvedStr);
  }
} // namespace Infra
