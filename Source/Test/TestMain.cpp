/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2024
 ***********************************************************************************************//**
 * @file TestMain.cpp
 *   Entry point for the test executable.
 **************************************************************************************************/

#include "Test/Harness.h"

int wmain(int argc, const wchar_t* argv[])
{
  return Infra::Test::Harness::RunTestsWithMatchingPrefix(((argc > 1) ? argv[1] : L""));
}
