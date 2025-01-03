/***************************************************************************************************
 * Infra
 *   Common infrastructure used by other projects.
 ***************************************************************************************************
 * Authored by Samuel Grossman
 * Copyright (c) 2016-2025
 ***********************************************************************************************//**
 * @file Mutex.h
 *   Declaration and implementation of simple wrappers around standard concurrency control
 *   mechanisms for higher performance on Windows.
 **************************************************************************************************/

#pragma once

#include <shared_mutex>

#include "ApiWindows.h"

namespace Infra
{
  /// Standard mutex, which is not recursive and must be exclusively locked. Meets all of the
  /// `Lockable` requirements for compatibility with all standard C++ lock objects. Implemented as
  /// a wrapper around `std::shared_mutex` but with the shared features removed for perfect
  /// compatibility with `std::mutex`. On Windows, `std::shared_mutex` is implemented using much
  /// more modern and performant mechanisms compared to `std::mutex`. For more information on
  /// performance, see
  /// https://stackoverflow.com/questions/69990339/why-is-stdmutex-so-much-worse-than-stdshared-mutex-in-visual-c
  /// The entire interface to this class mirrors that of `std::mutex`. See C++ standard
  /// documentation for more information.
  class Mutex
  {
  public:

    using native_handle_type = std::shared_mutex::native_handle_type;

  public:

    Mutex(void) = default;

    Mutex(const Mutex& other) = delete;

    ~Mutex(void) = default;

    inline void lock(void)
    {
      mutexObject.lock();
    }

    inline bool try_lock(void)
    {
      return mutexObject.try_lock();
    }

    inline void unlock(void)
    {
      mutexObject.unlock();
    }

    inline native_handle_type native_handle(void)
    {
      return mutexObject.native_handle();
    }

  private:

    std::shared_mutex mutexObject;
  };

  /// Recursive mutex, which can be locked multiple times by the same thread as long as it is
  /// unlocked the same number of times. Meets all of the `Lockable` requirements for
  /// compatibility with all standard C++ lock objects. Implemented as a wrapper around a
  /// CRITICAL_SECTION object, which is a re-entrant form of concurrency control object.
  class RecursiveMutex
  {
  public:

    using native_handle_type = void*;

  public:

    inline RecursiveMutex(void)
    {
      // Microsoft's own heap manager uses a spin count of 4000, which improves performance
      // for code sequences of short duration. The return value for
      // #InitializeCriticalSectionAndSpinCount is safe to ignore because on modern versions
      // of Windows this function always succeeds and returns non-zero. For more information,
      // see
      // https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-initializecriticalsectionandspincount
      std::ignore = InitializeCriticalSectionAndSpinCount(&criticalSectionObject, 4000);
    }

    RecursiveMutex(const RecursiveMutex& other) = delete;

    inline ~RecursiveMutex(void)
    {
      DeleteCriticalSection(&criticalSectionObject);
    }

    inline void lock(void)
    {
      EnterCriticalSection(&criticalSectionObject);
    }

    inline bool try_lock(void)
    {
      return (0 != TryEnterCriticalSection(&criticalSectionObject));
    }

    inline void unlock(void)
    {
      LeaveCriticalSection(&criticalSectionObject);
    }

    inline native_handle_type native_handle(void)
    {
      return &criticalSectionObject;
    }

  private:

    CRITICAL_SECTION criticalSectionObject;
  };

  /// Shared mutex, also known as a reader/writer mutex. The existing Windows implementation of
  /// std::shared_mutex uses SRWLock, which is as modern and performant as it gets, so no wrapping
  /// is necessary. For more information on performance, see
  /// https://stackoverflow.com/questions/69990339/why-is-stdmutex-so-much-worse-than-stdshared-mutex-in-visual-c
  using SharedMutex = std::shared_mutex;
} // namespace Infra
