/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file Harness.cpp
 *   Implementation of the test harness, which runs test cases and collects results.
 **************************************************************************************************/

#include "Test/Harness.h"

#include <map>
#include <set>
#include <string_view>

#include "Core/DebugAssert.h"
#include "Test/TestCase.h"
#include "Test/Utilities.h"

namespace Infra
{
  namespace Test
  {
    Harness& Harness::GetInstance(void)
    {
      static Harness harness;
      return harness;
    }

    void Harness::RegisterTestCaseInternal(const ITestCase* const testCase, std::wstring_view name)
    {
      if ((false == name.empty()) && (false == testCases.contains(name)))
        testCases[name] = testCase;
    }

    int Harness::RunTestsWithMatchingPrefixInternal(std::wstring_view prefixToMatch)
    {
      std::set<std::wstring_view> failingTests;
      unsigned int numExecutedTests = 0;
      unsigned int numSkippedTests = 0;

      switch (testCases.size())
      {
        case 0:
          Print(L"\nNo tests defined!\n");
          return -1;

        default:
          PrintFormatted(
              L"\n%u test%s defined.",
              static_cast<unsigned int>(testCases.size()),
              ((1 == testCases.size()) ? L"" : L"s"));
          break;
      }

      if (true == prefixToMatch.empty())
        Print(L"Running all tests.");
      else
        PrintFormatted(L"Running only tests with \"%s\" as a prefix.", prefixToMatch.data());

      Print(L"\n================================================================================");

      do
      {
        for (auto testCaseIterator = testCases.begin(); testCaseIterator != testCases.end();
             ++testCaseIterator)
        {
          const auto& name = testCaseIterator->first;
          const ITestCase* const testCase = testCaseIterator->second;

          if (false == name.starts_with(prefixToMatch)) continue;

          if (testCase->CanRun())
          {
            PrintFormatted(L"\n[ %-9s ] %s", L"RUN", name.data());

            bool testCasePassed = false;
            try
            {
              numExecutedTests += 1;

              testCase->Run();
              testCasePassed = true;
            }
            catch (const DebugAssertionException& assertion)
            {
              std::wstring_view assertionMessage = assertion.GetFailureMessage();
              PrintFormatted(
                  L"Uncaught debug assertion failure!\n%.*s",
                  static_cast<int>(assertionMessage.length()),
                  assertionMessage.data());
            }
            catch (const TestFailedException&)
            {}

            if (true != testCasePassed) failingTests.insert(name.data());

            PrintFormatted(
                L"[ %9s ] %s", (true == testCasePassed ? L"PASS" : L"FAIL"), name.data());
          }
          else
          {
            PrintFormatted(L"[  %-8s ] %s", L"SKIPPED", name.data());
            numSkippedTests += 1;
          }
        }
      }
      while (false);

      Print(L"\n================================================================================");

      if (numSkippedTests > 0)
        PrintFormatted(
            L"\nFinished running %u test%s (%u skipped).\n",
            numExecutedTests,
            ((1 == numExecutedTests) ? L"" : L"s"),
            numSkippedTests);
      else
        PrintFormatted(
            L"\nFinished running %u test%s.\n",
            numExecutedTests,
            ((1 == numExecutedTests) ? L"" : L"s"));

      const int numFailingTests = static_cast<int>(failingTests.size());

      if (numExecutedTests > 0)
      {
        if (testCases.size() == numSkippedTests)
        {
          Print(L"All tests skipped.\n");
        }
        else
        {
          switch (numFailingTests)
          {
            case 0:
              Print(L"All tests passed!\n");
              break;

            default:
              PrintFormatted(
                  L"%u test%s failed:", numFailingTests, ((1 == numFailingTests) ? L"" : L"s"));
              break;
          }
        }

        if (numFailingTests > 0)
        {
          for (std::wstring_view failingTest : failingTests)
            PrintFormatted(L"    %s", failingTest.data());

          Print(L"\n");
        }
      }
      else
      {
        Print(L"No results available.\n");
      }

      return numFailingTests;
    }
  } // namespace Test
} // namespace Infra
