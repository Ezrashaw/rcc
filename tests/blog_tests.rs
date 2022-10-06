use std::{
    fs::{self},
    path::Path,
};

#[test]
fn blog_tests() {
    for stage in fs::read_dir(Path::new("tests/blog_tests/"))
        .unwrap()
        .map(|d| d.unwrap())
    {
        for test in fs::read_dir(&stage.path().join("invalid/")).unwrap() {
            let result = compile(&test.as_ref().unwrap().path());
            if result.is_ok() {
                println!(
                    "Test: {:?} was meant to fail but succeeded.",
                    test.unwrap().path()
                );
            }
        }

        for test in fs::read_dir(&stage.path().join("valid/")).unwrap() {
            let result = compile(&test.as_ref().unwrap().path());
            if let Err(err) = result {
                println!("Test: {:?} failed:\n{}", test.unwrap().path(), err);
            }
        }
    }
}

fn compile(path: &Path) -> Result<(), rcc::error::CompileError> {
    rcc::compile(
        fs::read_to_string(path).unwrap(),
        Path::new("output"),
        "blog_post test".to_owned(),
        false,
    )?; // TODO: temporary name for test file passed to compiler

    fs::remove_file(Path::new("output")).unwrap();

    Ok(())
}
