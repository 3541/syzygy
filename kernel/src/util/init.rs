/*
 * INIT: Single-initialization.
 *
 * Copyright (c) 2026 Alex O'Brien <3541@3541.website>
 *
 * This file is part of Syzygy.
 *
 * Syzygy is free software: you can redistribute it and/or modify it under the
 * terms of version 3 the GNU General Public License as published by the Free
 * Software Foundation.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software. If not, see <https://www.gnu.org/licenses/>.
 */

use core::{
    cell::UnsafeCell,
    mem::MaybeUninit,
    sync::atomic::{AtomicU8, Ordering},
};

#[repr(u8)]
enum State {
    Uninitialized = 0,
    Initializing = 1,
    Initialized = 2,
}

pub struct InitOnce<T> {
    data: MaybeUninit<UnsafeCell<T>>,
    state: AtomicU8,
}

unsafe impl<T: Sync> Sync for InitOnce<T> {}

impl<T> InitOnce<T> {
    pub const fn new() -> Self {
        Self {
            data: MaybeUninit::uninit(),
            state: AtomicU8::new(State::Uninitialized as u8),
        }
    }

    pub fn init(&self, value: T) {
        let res = self.state.compare_exchange(
            State::Uninitialized as u8,
            State::Initializing as u8,
            Ordering::AcqRel,
            Ordering::Relaxed,
        );
        if !res.is_ok() {
            panic!("Multiple initialization of InitOnce.");
        }

        // SAFETY: Only one writer can claim the Initializing state, and no shared references are created until the Initialized state.
        unsafe { UnsafeCell::raw_get(self.data.as_ptr()).write(value) };

        self.state
            .store(State::Initialized as u8, Ordering::Release);
    }

    pub fn get(&self) -> &T {
        assert_eq!(self.state.load(Ordering::Acquire), State::Initialized as u8);
        // SAFETY: Once in initialized state, only shared references can exist.
        unsafe { self.data.assume_init_ref().get().as_ref_unchecked() }
    }
}

impl<T> Drop for InitOnce<T> {
    fn drop(&mut self) {
        let state = self.state.load(Ordering::Acquire);
        if state == State::Uninitialized as u8 {
            return;
        }
        if state == State::Initializing as u8 {
            panic!("Dropped mid-initialization.");
        }

        assert_eq!(state, State::Initialized as u8);
        unsafe { self.data.assume_init_drop() };
    }
}
