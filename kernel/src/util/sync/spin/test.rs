use std::thread;

use super::Spinlock;

#[test]
fn lock_and_release() {
    let lock = Spinlock::new(123);
    assert_eq!(*lock.lock(), 123);
    {
        *lock.lock() = 345;
    }
    assert_eq!(*lock.lock(), 345);
}

fn update_counter(counter: &Spinlock<usize>) {
    let mut lock = counter.lock();
    let val = *lock;
    assert_eq!(val, *lock);
    *lock += 1;
    assert_eq!(val + 1, *lock);
    *lock += 1;
    assert_eq!(val + 2, *lock);
}

#[test]
fn exclusion() {
    static COUNTER: Spinlock<usize> = Spinlock::new(0);

    static MAX: usize = 100000;
    static THREADS: usize = 3;

    let mut threads = Vec::with_capacity(THREADS);

    for _ in 0..THREADS {
        threads.push(thread::spawn(|| {
            for _ in 0..MAX {
                update_counter(&COUNTER)
            }
        }));
    }

    for t in threads {
        t.join();
    }

    assert_eq!(*COUNTER.lock(), 2 * MAX * THREADS);
}
