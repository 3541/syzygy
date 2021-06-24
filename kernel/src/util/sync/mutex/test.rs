//! Testing

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
