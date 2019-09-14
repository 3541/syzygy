pub fn runner(tests: &[&dyn Fn()]) {
    println!("Running {} tests", tests.len());
    for t in tests {
        t();
    }
}
