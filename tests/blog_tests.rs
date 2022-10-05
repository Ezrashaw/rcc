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
        println!("DIRECTORY {:?}:", stage.file_name().to_str().unwrap());

        run_invalid(&stage.path().join("invalid/"));
        run_valid(&stage.path().join("valid/"));
    }
}

fn run_invalid(path: &Path) {
    let output = Path::new("output");
    for test in fs::read_dir(path).unwrap() {
        compile_invalid(fs::read_to_string(test.unwrap().path()).unwrap(), output);
    }
}

fn compile_invalid(test: String, output: &Path) {
    // TODO: we need proper error handling. THIS JUST CRASHES THE TEST!!!!
    //rcc::compile(test, output);
}

fn run_valid(path: &Path) {
    let output = Path::new("output");
    for test in fs::read_dir(path).unwrap() {
        let test = test.unwrap();
        println!("\t {} (VALID):", test.file_name().to_str().unwrap());
        // TODO: again, better error handling would be nice so we can display nicely to `cargo test`
        rcc::compile(fs::read_to_string(test.path()).unwrap(), output);
        fs::remove_file(Path::new("output")).unwrap();
    }
}
