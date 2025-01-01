/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file ApiWindows.h
 *   Common header file for the correct version of the Windows API.
 **************************************************************************************************/

#pragma once

// Windows header files are sensitive to include order. Compilation will fail if the order is
// incorrect. Top-level macros and headers must come first, followed by headers for other parts
// of system functionality.

// clang-format off

#define NOMINMAX
#include <sdkddkver.h>
#include <windows.h>

// clang-format on

#include <knownfolders.h>
#include <shlobj.h>
