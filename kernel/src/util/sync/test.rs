//! Testing

use futures::future;

use super::Mutex;

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn acquire_and_release() {
    let mutex = Mutex::new(123);
    assert_eq!(*mutex.lock().await, 123);
    {
        *mutex.lock().await = 345;
    }
    assert_eq!(*mutex.lock().await, 345);
}

async fn update_counter(counter: &Mutex<usize>) {
    let mut lock = counter.lock().await;
    let val = *lock;
    assert_eq!(val, *lock);
    *lock += 1;
    assert_eq!(val + 1, *lock);
    *lock += 1;
    assert_eq!(val + 2, *lock);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 8)]
async fn exclusion() {
    static COUNTER: Mutex<usize> = Mutex::new(0);

    static MAX: usize = 100000;
    static THREADS: usize = 8;

    let mut handles = Vec::new();

    for _ in 0..THREADS {
        handles.push(tokio::spawn(update_counter(&COUNTER).await));
    }

    future::join_all(handles).await;

    assert_eq!(*COUNTER.lock().await, 2 * MAX * THREADS);
}
