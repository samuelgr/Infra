/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2026
 ***********************************************************************************************//**
 * @file TestMain.cpp
 *   Entry point for the test executable.
 **************************************************************************************************/

#include "Core/ProcessInfo.h"
#include "Test/Harness.h"

INFRA_DEFINE_PRODUCT_NAME_FROM_LITERAL(L"CoreInfra");
INFRA_DEFINE_PRODUCT_VERSION_FROM_GIT_VERSION_INFO();

int wmain(int argc, const wchar_t* argv[])
{
  return Infra::Test::Harness::RunTestsWithMatchingPrefix(((argc > 1) ? argv[1] : L""));
}
